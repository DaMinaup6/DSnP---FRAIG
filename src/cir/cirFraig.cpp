/****************************************************************************
  FileName     [ cirFraig.cpp ]
  PackageName  [ cir ]
  Synopsis     [ Define cir FRAIG functions ]
  Author       [ Chung-Yang (Ric) Huang ]
  Copyright    [ Copyleft(c) 2012-2014 LaDs(III), GIEE, NTU, Taiwan ]
****************************************************************************/

#include <cassert>
#include "cirMgr.h"
#include "cirGate.h"
#include "sat.h"
#include "myHashMap.h"
#include "util.h"

using namespace std;

// TODO: Please keep "CirMgr::strash()" and "CirMgr::fraig()" for cir cmd.
//       Feel free to define your own variables or functions

/************************************/
/*   HashKey class implementation   */
/************************************/
class StrashKey
{
public:
	/*   Initialization   */
	StrashKey(): _in0(0), _in1(0) {}
	StrashKey(CirGate* g) { 
		_in0 = (size_t)g->getInput0() + (size_t)g->getisInv0();
		_in1 = (size_t)g->getInput1() + (size_t)g->getisInv1();
	}
	~StrashKey() {}
	/*   Operator overloading   */
	void operator =  (const StrashKey& k) { _in0 = k._in0; _in1 = k._in1; }
	bool operator == (const StrashKey& k) const {
		return ((_in0 == k._in0 && _in1 == k._in1) || (_in0 == k._in1 && _in1 == k._in0));
	}
	size_t operator() () const {
		size_t ranNum = _in0 + _in1 + (_in0 % 256) * (_in1 % 256);
    	return ranNum;
	}
	/*   Debug helper functions   */
	const unsigned getInv0() const { return _in0 % sizeof(size_t) == 1; }
	const unsigned getInv1() const { return _in1 % sizeof(size_t) == 1; }

private:
	size_t   _in0;
	size_t   _in1;
};

/*******************************************/
/*   Public member functions about fraig   */
/*******************************************/
void
CirMgr::strash()
{
	if (_dfsList.size() == 0) cirDFSearch();
	bool resetDFSList = false;
	HashMap<StrashKey, CirGate*>* cirStrHash = new HashMap<StrashKey, CirGate*>;
	for (size_t i = 0; i < _dfsList.size(); ++i) {
		StrashKey tempKey(_dfsList[i]); CirGate* merGate = 0;
		if (_dfsList[i]->getTypeStr() == "AIG") {
			if (cirStrHash->check(tempKey, merGate)) {
				gateMerge(merGate, _dfsList[i], false);
				cout << "Strashing: " << merGate->getGateID() << " merging " << _dfsList[i]->getGateID() << "..." << endl;
				if (_dfsList[i]->getTypeStr() == "AIG") --_aigNum;
				_totList[_dfsList[i]->getGateID()] = 0;
				resetDFSList = true;
			}
			else cirStrHash->forceInsert(tempKey, _dfsList[i]);
		}
	}
	delete cirStrHash; cirStrHash = 0;
	
	if (resetDFSList) { _dfsList.resize(0); cirDFSearch(); }
}

void
CirMgr::fraig()
{
	if (_dfsList.size() == 0) cirDFSearch();
	_solver.initialize();
	genProofModel();
	solveFECPairs();
	fecGroNumReset();
	_fecGroSize = 0;
	_fecGroups.resize(0);
	strash();
}

/********************************************/
/*   Private member functions about fraig   */
/********************************************/
void
CirMgr::genProofModel()
{
	Var v0 = _solver.newVar();
	_totList[0]->setVar(v0);
	for (size_t i = 0; i < _dfsList.size(); ++i) {
		Var v = _solver.newVar();
		_dfsList[i]->setVar(v);
      if (_dfsList[i]->getTypeStr() == "AIG") {
      	bool inv0 = _dfsList[i]->getisInv0(); CirGate* inGate0 = _dfsList[i]->getInput0();
      	if (_dfsList[i]->getInput1()) {
      		bool inv1 = _dfsList[i]->getisInv1(); CirGate* inGate1 = _dfsList[i]->getInput1();
      		_solver.addAigCNF(_dfsList[i]->getVar(), inGate0->getVar(), inv0, inGate1->getVar(), inv1);
      	}
      	else _solver.addAigCNF(_dfsList[i]->getVar(), inGate0->getVar(), inv0, inGate0->getVar(), inv0);
      }
   }
}

void
CirMgr::solveFECPairs()
{
	bool repeat = true;
	while (repeat) {
		bool result = true;
		size_t round = 0;
		vector<size_t> simPattern;
		simPattern.resize(_piList.size());
		solveDFSList(simPattern, round, result);
		if (round < 32)
			while (round < 32) {
				for (size_t j = 0; j < _piList.size(); ++j)
					simPattern[j] |= ((rand() % 2) << round);
				++round;
				repeat = false;
			}
		if (!result) { _dfsList.resize(0); cirDFSearch(); _solver.initialize(); genProofModel(); }
		valPISim(simPattern, _piList); cirFileSim();
		fecReCheck(false, true, result);
		solveDFSList(simPattern, round, result);
	}
}

void
CirMgr::solveDFSList(vector<size_t>& simPattern, size_t& round, bool& result)
{
	for (size_t i = 0; i < _dfsList.size(); ++i) {
		if (!_dfsList[i]) continue;
		if (round == 32) break;
		string type = _dfsList[i]->getTypeStr();
		int groNum  = _dfsList[i]->getGroNum();
		size_t thisID = _dfsList[i]->getGateID();
		bool hasChecked = _dfsList[i]->hasFraiged();
		if (groNum != -1 && type != "PI" && type != "PO" && !hasChecked && _totList[thisID] && _totList[_fecGroups[groNum][0]] && _totList[_fecGroups[groNum][0]]->getGateID() != thisID) {
			Var newV = _solver.newVar();
			_totList[_fecGroups[groNum][0]]->setFraiged(true);
			CirGate* leadGate = _totList[_fecGroups[groNum][0]];
			size_t leadID = leadGate->getGateID();
			bool isInv = (_dfsList[i]->getGateValue() == leadGate->getGateValue() ? false : true);
			string tmp = (isInv ? "!" : "");
			_solver.addXorCNF(newV, leadGate->getVar(), false, _dfsList[i]->getVar(), isInv);
			_solver.assumeRelease();
			_solver.assumeProperty(_totList[0]->getVar(), false);
			_solver.assumeProperty(newV, true);
			if (!_solver.assumpSolve()) {
				gateMerge(leadGate, _dfsList[i], isInv);
				proveMessage(leadID, thisID, isInv, false);
				cout << "Fraig: " << leadID << " merging " << tmp << thisID << "..." << endl;
				if (type == "AIG") --_aigNum;
				_totList[thisID] = 0;
				result = false;
			}
			else {
				proveMessage(leadID, thisID, isInv, true);
				for (size_t j = 0; j < _piList.size(); ++j) {
					int bit = _solver.getValue(_piList[j]->getVar());
					if (bit == -1) bit = rand() % 2;
					simPattern[j] |= ((size_t)bit << round);
				}
				++round;
			}
		}
	}
}

void
CirMgr::proveMessage(size_t leadID, size_t thisID, bool isInv, bool sat)
{
	size_t stringSize = 0; string tmp = (isInv ? "!" : "");
	if (leadID != 0) {
		if (sat) {
			cout << "Proving (" << leadID << ", " << tmp << thisID << ")...SAT!!" << flush << "\r";
			stringSize = 20 + CirGate::myGNum2Str((int)leadID).size() + CirGate::myGNum2Str((int)thisID).size() + tmp.size();
		}
		else {
			cout << "Proving (" << leadID << ", " << tmp << thisID << ")...UNSAT!!" << flush << "\r";
			stringSize = 22 + CirGate::myGNum2Str((int)leadID).size() + CirGate::myGNum2Str((int)thisID).size() + tmp.size();
		}
		for (size_t j = 0; j < stringSize; ++j)
			cout << " ";
		cout << "\r";
	}
	else {
		size_t boolean = isInv ? 0 : 1;
		if (sat) {
			cout << "Proving " << thisID << " = " << boolean << "...SAT!!" << flush << "\r";
			stringSize = 20 + CirGate::myGNum2Str((int)thisID).size();
		}
		else {
			cout << "Proving " << thisID << " = " << boolean << "...UNSAT!!" << flush << "\r";
			stringSize = 22 + CirGate::myGNum2Str((int)thisID).size();
		}
		for (size_t j = 0; j < stringSize; ++j)
			cout << " ";
		cout << "\r";
	}
}