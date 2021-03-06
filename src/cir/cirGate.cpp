/****************************************************************************
  FileName     [ cirGate.cpp ]
  PackageName  [ cir ]
  Synopsis     [ Define class CirAIGGate member functions ]
  Author       [ Chung-Yang (Ric) Huang ]
  Copyright    [ Copyleft(c) 2008-2014 LaDs(III), GIEE, NTU, Taiwan ]
****************************************************************************/

#include <iostream>
#include <iomanip>
#include <sstream>
#include <stdarg.h>
#include <cassert>
#include <algorithm>
#include "cirGate.h"
#include "cirMgr.h"
#include "util.h"

using namespace std;

// TODO: Keep "CirGate::reportGate()", "CirGate::reportFanin()" and
//       "CirGate::reportFanout()" for cir cmds. Feel free to define
//       your own variables and functions.

/********************************/
/*   Global variable and enum   */
/********************************/
extern CirMgr *cirMgr;

/**************************************/
/*   Static varaibles and functions   */
/**************************************/
unsigned CirGate::_globalRef = 0;

/**************************************/
/*   class CirGate member functions   */
/**************************************/

/* Basic access methods */
string 
CirGate::getTypeStr() const 
{
	switch (_gateType) {
		case UNDEF_GATE: return "UNDEF";
		case PI_GATE:    return "PI";
		case PO_GATE:    return "PO";
		case AIG_GATE:   return "AIG";
		case CONST_GATE: return "CONST";
		default: break;
	}
	return ""; 
}

void
CirGate::setGateValue() 
{
	size_t in0Val = 0; size_t in1Val = 0;
	bool hasIn0 = this->getInput0(); bool hasIn1 = this->getInput1();
	bool in0Inv = this->getisInv0(); bool in1Inv = this->getisInv1();
	if (hasIn0) in0Val = this->getInput0()->getGateValue();
	if (hasIn1) in1Val = this->getInput1()->getGateValue();
	if (hasIn0 && !hasIn1) {
		if (in0Inv) in0Val = ~in0Val;
		setGateValue(in0Val); return;
	}
	else if (!hasIn0 && hasIn1) {
		if (in1Inv) in1Val = ~in1Val;
		setGateValue(in1Val); return;
	}
	else if (hasIn0 && hasIn1) {
		if (in0Inv) in0Val = ~in0Val;
		if (in1Inv) in1Val = ~in1Val;
		setGateValue(in0Val & in1Val);
		return;
	}
}

/* Printing functions */
void
CirGate::reportGate()
{
	string symbol = ""; string basStr = ""; string valStr = "= Value: "; string fecStr = "= FECs:";
	size_t val = getGateValue();
	/* Basic information */
	if (this->getSymbol() != "") symbol = "\"" + this->getSymbol() + "\"";
	basStr = "= " + this->getTypeStr() + "(" + myGNum2Str((int)(this->getGateID())) + ")"
				+ symbol + ", line " + myGNum2Str((int)(this->getLineNo()));
	/* FECs  information */
	if (_groNum != -1 && (cirMgr->_fecGroups)[(size_t)_groNum].size() >= 2) {
		for (size_t i = 0; i < (cirMgr->_fecGroups)[(size_t)_groNum].size(); ++i) {
			if ((cirMgr->_fecGroups)[(size_t)_groNum][i] != (int)_gateID && (cirMgr->_fecGroups)[(size_t)_groNum][i] != -1) {
				if (this->getGateValue() != cirMgr->_totList[(cirMgr->_fecGroups)[(size_t)_groNum][i]]->getGateValue()) {
					fecStr += " !"; fecStr += myGNum2Str((int)((cirMgr->_fecGroups)[(size_t)_groNum][i]));
				}
				else { fecStr += " "; fecStr += myGNum2Str((int)((cirMgr->_fecGroups)[(size_t)_groNum][i])); }
			}
		}
	}
	/* Value information */
	string valStrVal = "";
	for (size_t i = 0; i < 32; ++i) {
		if ((val & (1 << i)) == (size_t)(1 << i)) valStrVal += "1";
		else valStrVal += "0";
		if (i != 31 && (i + 1) % 4 == 0) valStrVal += "_";
	}
	reverse(valStrVal.begin(), valStrVal.end());
	valStr += valStrVal;
	/* Print information */
	cout << "==================================================" << endl;
	cout << setw(48) << left << basStr << right << " ="          << endl;
	cout << setw(48) << left << fecStr << right << " ="          << endl;
	cout << setw(48) << left << valStr << right << " ="          << endl;
	cout << "==================================================" << endl;
}

void
CirGate::reportFanin(int level)
{
   assert (level >= 0);
   CirGate::setGlobalRef();
   preOrder(this, 0, (unsigned)level + 1, false);
}

void
CirGate::reportFanout(int level)
{
   assert (level >= 0);
   CirGate::setGlobalRef();
   backPreOrder(this, 0, (unsigned)level + 1, false);
}

/* Searching functions */
void 
CirGate::preOrder(CirGate* n, unsigned layer, unsigned level, bool isInv) 
{
	if (n) {
		string temp = "";
		for (size_t i = 0; i < layer; ++i)
			cout << "  ";
		if (isInv) temp = "!";
		cout << temp << n->getTypeStr() << " " << n->getGateID();
		++layer;
		if (!n->eqGlobalRef()) {
			n->setToGlobalRef();
			cout << endl;
			if (level > layer && n->getTypeStr() == "PO") {
				n->setToOpen();
				isInv = n->getisInv0();
				preOrder(n->getInput0(), layer, level, isInv);
			}
			else if (level > layer && n->getTypeStr() == "AIG") {
				n->setToOpen();
				bool isInv0 = n->getisInv0();
				preOrder(n->getInput0(), layer, level, isInv0);
				if (n->getInput1()) {
					bool isInv1 = n->getisInv1();
					preOrder(n->getInput1(), layer, level, isInv1);
				}
			}
		}
		else if (n->eqGlobalRef() && n->prevIsVisted() && n->isOpened() && level > layer)
			cout << " (*)" << endl;
		else cout << endl;
	}
}

void 
CirGate::backPreOrder(CirGate* n, unsigned layer, unsigned level, bool isInv) 
{
	if (n) {
		string temp = "";
		for (size_t i = 0; i < layer; ++i)
			cout << "  ";
		if (isInv) temp = "!";
		cout << temp << n->getTypeStr() << " " << n->getGateID();
		++layer;
		if (!n->eqGlobalRef()) {
			cout << endl;
			n->setToGlobalRef();
			if (level > layer) {
				n->setToOpen();
				GateList tempOut = n->getOutput();
				if (tempOut.size() == 0) return;
				for (size_t i = 0; i < tempOut.size(); ++i) {
					isInv = n->getOutInv(tempOut[i]);
					backPreOrder(tempOut[i], layer, level, isInv);
				}
			}
		}
		else if (n->eqGlobalRef() && n->nextIsVisted() && n->isOpened() && level > layer)
			cout << " (*)" << endl;
		else cout << endl;
	}
}

bool 
CirGate::prevIsVisted() 
{
	if (this->getInput0()) return this->getInput0()->eqGlobalRef(); 
	else return false;
}

bool 
CirGate::nextIsVisted() 
{
	if (this->getOutput().size() != 0) return this->getOutput()[0]->eqGlobalRef(); 
	else return false;
}

/* Input & output functions */
bool 
CirGate::getOutInv(CirGate* out) 
{
	GateList tempOut = this->getOutput();
	if (tempOut.size() == 0) return false;
	for (size_t i = 0; i < tempOut.size(); ++i) {
		if (tempOut[i]->getInput0() && tempOut[i]->getInput0()->getGateID() == this->getGateID() && tempOut[i] == out)
			return tempOut[i]->getisInv0();
		else if (tempOut[i]->getInput1() && tempOut[i]->getInput1()->getGateID() == this->getGateID() && tempOut[i] == out)
			return tempOut[i]->getisInv1();
	}
	return false;
}

/* Helper functions */
string
CirGate::myGNum2Str(int num)
{
	string baseVec;
	int base = 10;
	int baseNum = 0;
	int sign = 1;
	if (num >= 0) baseNum = num;
	else { baseNum = num * (-1); sign = -1; }
	
	if (baseNum == 0) { baseVec = "0"; }
	
	while (baseNum > 0) {
		int baseGNum = baseNum % base;
		if (baseGNum >= 10) baseVec.insert(0, 1, char(baseGNum - 10 + int('a')));
		else baseVec.insert(0, 1, char(baseGNum + int('0')));
		baseNum /= base;
	}
	
	if (sign == 1) return baseVec;
	else if (sign == -1) {
		if (baseVec == "0") return baseVec;
		else { baseVec.insert(0, 1, '-'); return baseVec; }
	}
	
	return baseVec;
}

/*****************************************/
/*   class CirAIGGate member functions   */
/*****************************************/

/* Searching functions */
void
CirAIGGate::gateDFSearch()
{
	this->setToGlobalRef();
	if (_aigIn0 && !_aigIn0->eqGlobalRef())
		_aigIn0->gateDFSearch();
	if (_aigIn1 && !_aigIn1->eqGlobalRef())
		_aigIn1->gateDFSearch();
	cirMgr->_dfsList.push_back(this);
}

/* Input & output functions */
void 
CirAIGGate::setInput(CirGate* inGate0, bool phase0, CirGate* inGate1, bool phase1) 
{ 
	this->setInv0(phase0); this->setInv1(phase1);
	_aigIn0 = inGate0; _aigIn1 = inGate1;
}

void 
CirAIGGate::removeOutput(CirGate* rmGate)
{ 
	_aigOut.erase(remove(_aigOut.begin(), _aigOut.end(), rmGate), _aigOut.end());
}

/* Simulation functions */
void
CirAIGGate::gateSim()
{
	this->setToGlobalRef();
	if (_aigIn0 && !_aigIn0->eqGlobalRef())
		_aigIn0->gateSim();
	if (_aigIn1 && !_aigIn1->eqGlobalRef())
		_aigIn1->gateSim();
	this->setGateValue();
}

/****************************************/
/*   class CirPIGate member functions   */
/****************************************/

/* Searching functions */
void
CirPIGate::gateDFSearch()
{
	this->setToGlobalRef();
	cirMgr->_dfsList.push_back(this);
}

/* Input & output functions */
void 
CirPIGate::removeOutput(CirGate* rmGate) 
{ 
	_piOut.erase(remove(_piOut.begin(), _piOut.end(), rmGate), _piOut.end());
}

/* Simulation functions */
void
CirPIGate::gateSim()
{
	this->setToGlobalRef();
}

/****************************************/
/*   class CirPOGate member functions   */
/****************************************/

/* Searching functions */
void
CirPOGate::gateDFSearch()
{
	this->setToGlobalRef();
	if (_poIn && !_poIn->eqGlobalRef())
		_poIn->gateDFSearch();
	cirMgr->_dfsList.push_back(this);
}

/* Input & output functions */
void 
CirPOGate::setInput(CirGate* inGate0, bool phase0, CirGate* inGate1, bool phase1)
{ 
	if (inGate0) { this->setInv0(phase0); _poIn = inGate0; }
	else if (inGate1) { this->setInv1(phase1); _poIn = inGate1; }
}

/* Simulation functions */
void
CirPOGate::gateSim()
{
	this->setToGlobalRef();
	if (_poIn && !_poIn->eqGlobalRef())
		_poIn->gateSim();
	this->setGateValue();
	cirMgr->_poValVec.push_back(this->getGateValue());
}

/*******************************************/
/*   class CirUndefGate member functions   */
/*******************************************/

/* Searching functions */
void
CirUndefGate::gateDFSearch()
{
	this->setToGlobalRef();
}

/* Input & output functions */
void
CirUndefGate::removeOutput(CirGate* rmGate) 
{
	_unOut.erase(remove(_unOut.begin(), _unOut.end(), rmGate), _unOut.end());
}

/* Simulation functions */
void
CirUndefGate::gateSim()
{
	this->setToGlobalRef();
	this->setGateValueTo0();
}

/*******************************************/
/*   class CirConstGate member functions   */
/*******************************************/

/* Searching functions */
void
CirConstGate::gateDFSearch()
{
	this->setToGlobalRef();
	cirMgr->_dfsList.push_back(this);
}

/* Input & output functions */
void 
CirConstGate::removeOutput(CirGate* rmGate)
{
	_consOut.erase(remove(_consOut.begin(), _consOut.end(), rmGate), _consOut.end());
}

/* Simulation functions */
void
CirConstGate::gateSim()
{
	this->setToGlobalRef();
	this->setGateValueTo0();
}