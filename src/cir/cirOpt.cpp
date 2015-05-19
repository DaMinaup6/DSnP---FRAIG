/****************************************************************************
  FileName     [ cirOpt.cpp ]
  PackageName  [ cir ]
  Synopsis     [ Define cir optimization functions ]
  Author       [ Chung-Yang (Ric) Huang ]
  Copyright    [ Copyleft(c) 2008-2014 LaDs(III), GIEE, NTU, Taiwan ]
****************************************************************************/

#include <cassert>
#include <vector>
#include <algorithm>
#include "cirMgr.h"
#include "cirGate.h"
#include "util.h"

using namespace std;

// TODO: Please keep "CirMgr::sweep()" and "CirMgr::optimize()" for cir cmd.
//       Feel free to define your own variables or functions

/**************************************************/
/*   Public member functions about optimization   */
/**************************************************/
void
CirMgr::sweep()
{
	_dfsList.resize(0); cirDFSearch();
	// Sweep floating gates
	for (size_t i = 0; i < _totList.size(); ++i) {
		if (_totList[i] && !_totList[i]->eqGlobalRef()) {
			if (_totList[i]->getTypeStr() == "AIG") {
				if (_totList[i]->getInput0()) sweepClearOut(_totList[i], 0);
				if (_totList[i]->getInput1()) sweepClearOut(_totList[i], 1);
				--_aigNum;
				_totList[i] = 0;
				cout << "Sweeping: AIG(" << i << ") removed..." << endl;
			}
			else if (_totList[i]->getTypeStr() == "UNDEF") {
				_undefList.erase(remove(_undefList.begin(), _undefList.end(), _totList[i]), _undefList.end());
				_totList[i] = 0;
				cout << "Sweeping: UNDEF(" << i << ") removed..." << endl;
			}
		}
	}
}

void
CirMgr::optimize()
{
	if (_dfsList.size() == 0) cirDFSearch();
	bool resetDFSList = false;
	for (size_t i = 0; i < _dfsList.size(); ++i) {
		if (_dfsList[i]->getInput0() && _dfsList[i]->getTypeStr() != "PO") {
			// Identical fanins
			if (_dfsList[i]->getInput1() && _dfsList[i]->getInput0() == _dfsList[i]->getInput1()) {
				// Identical phases
				if (_dfsList[i]->getisInv0() == _dfsList[i]->getisInv1())
					resetDFSList = optGateMerge(_dfsList[i], 0, false);
				// Inverted phases
				else resetDFSList = optGateMerge(_dfsList[i], 0, true);
			}
			// has constant 0
			else if (_dfsList[i]->getInput0()->getGateID() == 0 && !_dfsList[i]->getisInv0())
				resetDFSList = optGateMerge(_dfsList[i], 0, false);
			else if (_dfsList[i]->getInput1() && _dfsList[i]->getInput1()->getGateID() == 0 && !_dfsList[i]->getisInv1())
				resetDFSList = optGateMerge(_dfsList[i], 1, false);
			// has constant 1
			else if (_dfsList[i]->getInput0()->getGateID() == 0 && _dfsList[i]->getisInv0()) {
				if (_dfsList[i]->getInput1()) resetDFSList = optGateMerge(_dfsList[i], 1, false);
				else resetDFSList = optGateMerge(_dfsList[i], 0, false);
			}
			else if (_dfsList[i]->getInput1() && _dfsList[i]->getInput1()->getGateID() == 0 && _dfsList[i]->getisInv1())
				resetDFSList = optGateMerge(_dfsList[i], 0, false);
		}
	}
	
	if (resetDFSList) { _dfsList.resize(0); cirDFSearch(); }
}

/***************************************************/
/*   Private member functions about optimization   */
/***************************************************/
void
CirMgr::sweepClearOut(CirGate* gate, size_t pin)
{
	GateList outputList; CirGate* inGate;
	if (pin == 0) { outputList = gate->getInput0()->getOutput(); inGate = gate->getInput0(); }
	else { outputList = gate->getInput1()->getOutput(); inGate = gate->getInput1();}
	if (outputList.size() != 0)
		for (size_t i = 0; i < outputList.size(); ++i)
			if (!outputList[i]->eqGlobalRef())
				inGate->removeOutput(outputList[i]);
}

void
CirMgr::optDelGate(CirGate* gate)
{
	if (gate->getTypeStr() != "PO") {
		if (gate->getTypeStr() == "AIG") --_aigNum;
		_totList[gate->getGateID()] = 0;
	}
}

bool
CirMgr::optGateMerge(CirGate* gate, size_t pin, bool isConst0)
{
	string tmp = ""; bool inv = false; CirGate* inGate;
	if (!isConst0) {
		if (pin == 0) { inv = gate->getisInv0(); inGate = gate->getInput0(); }
		else { inv = gate->getisInv1(); inGate = gate->getInput1(); }
		tmp = inv ? "!" : "";
	}
	else inGate = _totList[0];
	gateMerge(inGate, gate, false); optDelGate(gate);
	cout << "Simplifying: " << inGate->getGateID() << " merging " << tmp << gate->getGateID() << "..." << endl;
	return true;
}