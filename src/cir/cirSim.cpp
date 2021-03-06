/****************************************************************************
  FileName     [ cirSim.cpp ]
  PackageName  [ cir ]
  Synopsis     [ Define cir simulation functions ]
  Author       [ Chung-Yang (Ric) Huang ]
  Copyright    [ Copyleft(c) 2008-2014 LaDs(III), GIEE, NTU, Taiwan ]
****************************************************************************/

#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <algorithm>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <unistd.h>
#include "cirMgr.h"
#include "cirGate.h"
#include "myHashMap.h"
#include "util.h"

using namespace std;

// TODO: Keep "CirMgr::randimSim()" and "CirMgr::fileSim()" for cir cmd.
//       Feel free to define your own variables or functions

/************************************/
/*   HashKey class implementation   */
/************************************/
class SimKey
{
public:
	/*   Initialization   */
	SimKey(CirGate* g) { _val = g->getGateValue(); }
	~SimKey() {}
	/*   Operator overloading   */
	void operator =  (const SimKey& k) { _val = k._val; }
	bool operator == (const SimKey& k) const {
		if (_val == k._val || _val == ~(k._val)) return true;
		else return false;
	}
	size_t operator() () const { 
		if (_val > ~(_val)) return ~(_val);
		else return _val; 
	}
	/*   Debug helper functions   */
	size_t getVal() { return _val; }

private:
	size_t   _val;
};

/************************************************/
/*   Public member functions about Simulation   */
/************************************************/
void
CirMgr::randomSim()
{
	if (_dfsList.size() == 0) cirDFSearch();
	valRecordClear();
	size_t round = sqrt(_andGateNum); size_t cycle = 0; size_t pattern = 0; bool first = true;
	if (round > 250) round = round * 2 / 3;
	if (_fecGroups.size() == 0) {
		cout << "MAX_FAILS = " << round << "\n";
		randPrint();
		if (round == 0) { cirRandSim(); ++pattern; ++cycle; }
		while (cycle < round) {
			if (first) { 
				cirRandSim(); ++pattern;
				if(!fecCheck(_dfsList, true)) break; 
				first = false; 
			}
			else {
				cirRandSim(); ++pattern;
				if (!fecReCheck(true, false, false)) ++cycle;
			}
		}
	}
	else {
		if (round > 165) round /= 3;
		cout << "MAX_FAILS = " << round << "\n";
		randPrint();
		while (cycle < round) {
			cirRandSim(); ++pattern;
			if (!fecReCheck(true, false, false)) ++cycle;
		}
	}
	fecGroNumSet();
	cout << pattern * 32 << " patterns simulated." << endl;
	_randSim = true;
}

void
CirMgr::fileSim(ifstream& patternFile)
{
	if(!patternFile) {
		cerr << "Cannot open design \"" << patternFile << "\"!!" << endl;
		return;
	}
	vector<string> simPattern;
   if (!fileCheck(patternFile, simPattern)) return;
   
   if (_dfsList.size() == 0) cirDFSearch();
   valRecordClear();
   /*   Simulate input pattern   */
   size_t round = simPattern.size() / 32; bool oneMoreRound = (simPattern.size() % 32 != 0);
   bool checkOnce = false;
   cout << "\r";
   for (size_t i = 0; i < round; ++i) {
   	size_t begin = 32 * i; size_t end = 32 * i + 31;
   	filePISim(simPattern, begin, end); cirFileSim();
		if (!checkOnce && _fecGroups.size() == 0) { fecCheck(_dfsList, false); checkOnce = true; }
		else fecReCheck(false, false, false);
   }
   if (oneMoreRound) {
   	size_t begin = 32 * round; size_t end = 32 * round + (simPattern.size() % 32) - 1;
   	filePISim(simPattern, begin, end); cirFileSim();
		if (!checkOnce && _fecGroups.size() == 0) { fecCheck(_dfsList, false); checkOnce = true; }
		else fecReCheck(false, false, false);
   }
   fecGroNumSet(); _valPattern = simPattern;
   cout << simPattern.size() << " patterns simulated." << endl;
   _randSim = false;
}

void
CirMgr::setSimLog(ofstream *logFile)
{
	if (_simLog) {
		if (_randSim) randWriteValToStr();
		else fileWriteValToStr();
		for (size_t i = 0; i < _valPattern.size(); ++i)
			(*_simLog) << _valPattern[i] << '\n';
	}
	_simLog = logFile;
}

/*************************************************/
/*   Private member functions about Simulation   */
/*************************************************/
bool
CirMgr::fileCheck(ifstream& patternFile, vector<string>& simPattern)
{
	string strBuf;
   while (getline(patternFile, strBuf)) {
   	/*   trim input string   */
   	size_t begin = strBuf.find_first_not_of(" \t");
   	size_t end   = strBuf.find_last_not_of(" \t");
   	if (strBuf.size() >= 2) { strBuf = strBuf.substr(begin, end - begin + 1); }
   	else if (strBuf.size() == 1 && (strBuf[0] == ' ' || strBuf[0] == '\t')) continue;
   	else if (strBuf.size() == 0) continue;
   	for (size_t i = 0; i < strBuf.size(); ++i)
   		if (strBuf[i] == '\t') strBuf[i] = ' ';
   	/*   check if string format is right   */
   	vector<string> tokens; mulTokens(strBuf, tokens);
   	for (size_t i = 0; i < tokens.size(); ++i) {
   		/*   check size   */
			if (tokens[i].size() != _piList.size()) {
				cout << endl;
				cerr << "Error: Pattern(" << tokens[i] << ") length(" << tokens[i].size()
					  << ") does not match the number of inputs("      << _piList.size()
					  << ") in a circuit!!" << endl;
				cout << "0 patterns simulated." << endl;
				return false;
			}
			/*   check if string contains non-0/1 character   */
   		for (size_t j = 0; j < tokens[i].size(); ++j) {
				if (tokens[i][j] != '0' && tokens[i][j] != '1') {
					cout << endl;
					cerr << "Error: Pattern(" << tokens[i] << ") contains a non-0/1 character('" 
						  << tokens[i][j] << "')." << endl;
					cout << "0 patterns simulated." << endl;
					return false;
				}
   		}
   		/*   input pattern   */
   		simPattern.push_back(tokens[i]);
   	}
   }
   patternFile.close(); // close the file
   return true;
}

void
CirMgr::cirRandSim()
{
	CirGate::setGlobalRef();
	/* Set value vector of PIs */
	for (size_t i = 0; i < _piList.size(); ++i) {
		size_t val = rand();
		_piList[i]->setGateValue(val);
		_piValVec.push_back(val);
	}
	/* Set value vector of gates of DFS list */
	if (_poList.size() != 0)
		for (unsigned i = 0; i < _poList.size(); ++i)
			_poList[i]->gateSim();
	else return;
}

void
CirMgr::cirFileSim()
{
	CirGate::setGlobalRef();
	/* Set value vector of gates of DFS list */
	if (_poList.size() != 0)
		for (unsigned i = 0; i < _poList.size(); ++i)
			_poList[i]->gateSim();
	else return;
}

void
CirMgr::valRecordClear()
{
	_piValVec.resize(0); _poValVec.resize(0);
}

void
CirMgr::filePISim(vector<string>& simPattern, size_t begin, size_t end)
{
	for (size_t i = 0; i < _piList.size(); ++i) {
		size_t val = 0;
		for (size_t j = begin; j <= end; ++j)
			val = val | ((simPattern[j][i] & 1) << (j % 32));
		_piList[i]->setGateValue(val);
		_piValVec.push_back(val);
	}
}

void
CirMgr::valPISim(vector<size_t>& simPattern, GateList& simPIs)
{
	for (size_t i = 0; i < simPIs.size(); ++i)
		simPIs[i]->setGateValue(simPattern[i]);
}

void
CirMgr::cycleWrite(vector<string>& tempPattern, vector<size_t> valVec, size_t cycMul, size_t patBegin)
{
	size_t round = valVec.size() / cycMul;
	size_t cycle = 0;
	while (cycle < round) {
		size_t begin = cycle * cycMul; size_t end = ((cycle + 1) * cycMul);
		for (size_t i = begin; i < end; ++i) {
			string tempStr = "";
			for (size_t k = 0; k < 32; ++k) {
				if ((valVec[i] & (1 << k)) == (size_t)(1 << k)) tempStr += "1";
				else tempStr += "0";
			}
			tempPattern[patBegin + (i % cycMul)] += tempStr;
		}
		++cycle;
	}
}

void
CirMgr::randWriteValToStr()
{
	_valPattern.resize(0);
	_valPattern.resize(_piList.size() + _poList.size() + 1);
	size_t piSize = _piList.size();
	size_t poSize = _poList.size();
	cycleWrite(_valPattern, _piValVec, piSize, 0);
	string spaceStr = "";
	for (size_t i = 0; i < _valPattern[0].size(); ++i) spaceStr += " ";
	_valPattern[piSize] += spaceStr;
	cycleWrite(_valPattern, _poValVec, poSize, piSize + 1);
	strTranpose(_valPattern);
}

void
CirMgr::fileWriteValToStr()
{
	size_t poSize = _poList.size();
	vector<string> tempPattern; tempPattern.resize(poSize);
	cycleWrite(tempPattern, _poValVec, poSize, 0);
	strTranpose(tempPattern);
	for (size_t k = 0; k < _valPattern.size(); ++k)
		_valPattern[k] = _valPattern[k] + " " + tempPattern[k];
}

void
CirMgr::strTranpose(vector<string>& tempPattern)
{
	vector<string> ttempPattern;
	for (size_t i = 0; i < tempPattern[0].size(); ++i) {
		string tempStr = "";
		for (size_t j = 0; j < tempPattern.size(); ++j)
			tempStr += tempPattern[j][i];
		ttempPattern.push_back(tempStr);
	}
	tempPattern = ttempPattern;
}

bool
CirMgr::fecCheck(GateList gateList, bool isRandom)
{
	bool hasFEC = false;
	HashMap<SimKey, intIdList>* cirSimHash = new HashMap<SimKey, intIdList>;
	for (size_t i = 0; i < gateList.size(); ++i) {
		if (!gateList[i]) continue;
		SimKey tempKey(gateList[i]); intIdList fecList;
		if ((gateList[i]->getTypeStr() == "AIG" || gateList[i]->getTypeStr() == "CONST") && !gateList[i]->hasFraiged()) {
			if (cirSimHash->check(tempKey, fecList)) {
				fecList.push_back(gateList[i]->getGateID());
				cirSimHash->replaceInsert(tempKey, fecList);
				hasFEC = true;
			}
			else { 
				fecList.push_back(gateList[i]->getGateID());
				cirSimHash->forceInsert(tempKey, fecList); 
			}
		}
	}
	fecGroupWrite(cirSimHash);
	delete cirSimHash;
	if (isRandom) randPrint();
	else filePrint();
	return hasFEC;
}

bool
CirMgr::fecReCheck(bool isRandom, bool isFraig, bool sat)
{
	bool hasDiff = false;
	for (size_t i = 0; i < _fecGroups.size(); ++i) {
		if (_fecGroups[i][0] != -1 && _totList[_fecGroups[i][0]]) {
			intIdList rmList; size_t nonNeg = 0;
			size_t leadVal = _totList[_fecGroups[i][0]]->getGateValue();
			for (size_t j = 1; j < _fecGroups[i].size(); ++j)
				if (_fecGroups[i][j] != -1) {
					if (_totList[_fecGroups[i][j]] == 0) _fecGroups[i][j] = -1;
					else {
						if (!_totList[_fecGroups[i][j]]->hasFraiged()) {
							size_t thisVal = _totList[_fecGroups[i][j]]->getGateValue();
							if (thisVal != leadVal && thisVal != ~leadVal) {
								rmList.push_back(_fecGroups[i][j]);
								rmFromFECList(i, j);
								hasDiff = true;
							}
							else ++nonNeg;
						}
						else rmFromFECList(i, j);
					}
				}
			if (nonNeg < 1) { _totList[(size_t)_fecGroups[i][0]]->setFraiged(true); rmFromFECList(i, 0); --_fecGroSize; }
			if (rmList.size() >= 2) { _fecGroups.push_back(rmList); ++_fecGroSize; }
			else if (rmList.size() == 1 && _totList[(size_t)rmList[0]]) _totList[(size_t)rmList[0]]->setFraiged(true);
		}
	}
	if (!isFraig) {
		if (isRandom) randPrint();
		else filePrint();
	}
	else fraigPrint(sat);
	return hasDiff;
}

void
CirMgr::fecGroupWrite(HashMap<SimKey, intIdList>*& cirSimHash)
{
	for (HashMap<SimKey, intIdList>::iterator hi = cirSimHash->begin(); hi != cirSimHash->end(); ++hi) {
		intIdList tempList = (*hi).second;
		if (tempList.size() >= 2 && _totList[tempList[0]]) {
			if (_totList[tempList[0]]->getGateValue() == 0 || _totList[tempList[0]]->getGateValue() == (size_t)(~0))
				if (find(tempList.begin(), tempList.end(), 0) == tempList.end() && !_totList[0]->hasFraiged())
					tempList.push_back(0);
			sort(tempList.begin(), tempList.end());
			_fecGroups.push_back(tempList);
			++_fecGroSize;
		}
		else if (tempList.size() == 1 && _totList[tempList[0]]) _totList[(size_t)tempList[0]]->setFraiged(true);
   }
}

void
CirMgr::fecGroNumSet()
{
	for (size_t i = 0; i < _fecGroups.size(); ++i) {
		IdList tempList;
		if (_fecGroups[i][0] != -1 && _fecGroups[i].size() >= 2) {
			for (size_t j = 0; j < _fecGroups[i].size(); ++j)
				if (_fecGroups[i][j] != -1) tempList.push_back((size_t)_fecGroups[i][j]);
			if (tempList.size() >= 2)
				for (size_t j = 0; j < tempList.size(); ++j)
					if (_totList[tempList[j]])
						_totList[tempList[j]]->setGroNum((int)i);
   	}
   }
}

void
CirMgr::fecGroNumReset()
{
	for (size_t i = 0; i < _fecGroups.size(); ++i)
		if (_fecGroups[i][0] != -1)
			for (size_t j = 0; j < _fecGroups[i].size(); ++j)
				if (_fecGroups[i][j] != -1 && _totList[_fecGroups[i][j]])
					_totList[_fecGroups[i][j]]->setGroNum(-1);
}

void
CirMgr::fecGroupClean()
{
	if (_fecGroups.size() != 0) {
		fecGroNumReset();
		_fecGroups.resize(0);
	}
	else return;
}

void
CirMgr::rmFromFECList(size_t i, size_t j)
{
	if (_totList[_fecGroups[i][j]])
		_totList[_fecGroups[i][j]]->setGroNum(-1);
	_fecGroups[i][j] = -1;
}

void
CirMgr::randPrint()
{
	cout << "Total #FEC Group = " << _fecGroSize << flush << "\r";
	for (size_t i = 0; i < (19 + CirGate::myGNum2Str((int)_fecGroSize).size()); ++i)
		cout << " ";
	cout << "\r";
}

void
CirMgr::filePrint()
{
	cout << "Total #FEC Group = " << _fecGroSize << flush << "\r";
}

void
CirMgr::fraigPrint(bool sat)
{
	if (sat) cout << "Updating by SAT... Total #FEC Group = " << _fecGroSize << "\n";
	else cout << "Updating by UNSAT... Total #FEC Group = " << _fecGroSize << "\n";
}