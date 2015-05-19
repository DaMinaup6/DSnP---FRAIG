/****************************************************************************
  FileName     [ cirMgr.cpp ]
  PackageName  [ cir ]
  Synopsis     [ Define cir manager functions ]
  Author       [ Chung-Yang (Ric) Huang ]
  Copyright    [ Copyleft(c) 2008-2014 LaDs(III), GIEE, NTU, Taiwan ]
****************************************************************************/

#include <iostream>
#include <fstream>
#include <iomanip>
#include <algorithm>
#include <vector>
#include <cstdio>
#include <ctype.h>
#include <cassert>
#include <cstring>
#include "cirMgr.h"
#include "cirGate.h"
#include "util.h"

using namespace std;

// TODO: Implement memeber functions for class CirMgr

/*******************************/
/*   Global variable and enum  */
/*******************************/
CirMgr* cirMgr = 0;

enum CirParseError {
   EXTRA_SPACE,
   MISSING_SPACE,
   ILLEGAL_WSPACE,
   ILLEGAL_NUM,
   ILLEGAL_IDENTIFIER,
   ILLEGAL_SYMBOL_TYPE,
   ILLEGAL_SYMBOL_NAME,
   MISSING_NUM,
   MISSING_IDENTIFIER,
   MISSING_NEWLINE,
   MISSING_DEF,
   CANNOT_INVERTED,
   MAX_LIT_ID,
   REDEF_GATE,
   REDEF_SYMBOLIC_NAME,
   REDEF_CONST,
   NUM_TOO_SMALL,
   NUM_TOO_BIG,

   DUMMY_END
};

/**************************************/
/*   Static varaibles and functions   */
/**************************************/
static unsigned lineNo = 0;  // in printint, lineNo needs to ++
static unsigned colNo  = 0;  // in printing, colNo needs to ++
static string errMsg;
static int errInt;
static CirGate *errGate;

static bool
parseError(CirParseError err)
{
   switch (err) {
      case EXTRA_SPACE:
         cerr << "[ERROR] Line " << lineNo+1 << ", Col " << colNo+1 << ": Extra space character is detected!!" << endl;
         break;
      case MISSING_SPACE:
         cerr << "[ERROR] Line " << lineNo+1 << ", Col " << colNo+1 << ": Missing space character!!" << endl;
         break;
      case ILLEGAL_WSPACE: // for non-space white space character
         cerr << "[ERROR] Line " << lineNo+1 << ", Col " << colNo+1 << ": Illegal white space char(" << errInt << ") is detected!!" << endl;
         break;
      case ILLEGAL_NUM:
         cerr << "[ERROR] Line " << lineNo+1 << ": Illegal " << errMsg << "!!" << endl;
         break;
      case ILLEGAL_IDENTIFIER:
         cerr << "[ERROR] Line " << lineNo+1 << ": Illegal identifier \"" << errMsg << "\"!!" << endl;
         break;
      case ILLEGAL_SYMBOL_TYPE:
         cerr << "[ERROR] Line " << lineNo+1 << ", Col " << colNo+1 << ": Illegal symbol type (" << errMsg << ")!!" << endl;
         break;
      case ILLEGAL_SYMBOL_NAME:
         cerr << "[ERROR] Line " << lineNo+1 << ", Col " << colNo+1 << ": Symbolic name contains un-printable char(" << errInt << ")!!" << endl;
         break;
      case MISSING_NUM:
         cerr << "[ERROR] Line " << lineNo+1 << ", Col " << colNo+1 << ": Missing " << errMsg << "!!" << endl;
         break;
      case MISSING_IDENTIFIER:
         cerr << "[ERROR] Line " << lineNo+1 << ": Missing \"" << errMsg << "\"!!" << endl;
         break;
      case MISSING_NEWLINE:
         cerr << "[ERROR] Line " << lineNo+1 << ", Col " << colNo+1 << ": A new line is expected here!!" << endl;
         break;
      case MISSING_DEF:
         cerr << "[ERROR] Line " << lineNo+1 << ": Missing " << errMsg << " definition!!" << endl;
         break;
      case CANNOT_INVERTED:
         cerr << "[ERROR] Line " << lineNo+1 << ", Col " << colNo+1 << ": " << errMsg << " " << errInt << "(" << errInt/2
              << ") cannot be inverted!!" << endl;
         break;
      case MAX_LIT_ID:
         cerr << "[ERROR] Line " << lineNo+1 << ", Col " << colNo+1 << ": Literal \"" << errInt << "\" exceeds maximum valid ID!!" << endl;
         break;
      case REDEF_GATE:
         cerr << "[ERROR] Line " << lineNo+1 << ": Literal \"" << errInt << "\" is redefined, previously defined as "
              << errGate->getTypeStr() << " in line " << errGate->getLineNo() << "!!" << endl;
         break;
      case REDEF_SYMBOLIC_NAME:
         cerr << "[ERROR] Line " << lineNo+1 << ": Symbolic name for \"" << errMsg << errInt << "\" is redefined!!" << endl;
         break;
      case REDEF_CONST:
         cerr << "[ERROR] Line " << lineNo+1 << ", Col " << colNo+1 << ": Cannot redefine const (" << errInt << ")!!" << endl;
         break;
      case NUM_TOO_SMALL:
         cerr << "[ERROR] Line " << lineNo+1 << ": " << errMsg << " is too small (" << errInt << ")!!" << endl;
         break;
      case NUM_TOO_BIG:
         cerr << "[ERROR] Line " << lineNo+1 << ": " << errMsg << " is too big (" << errInt << ")!!" << endl;
         break;
      default: break;
   }
   return false;
}

/**************************************************************/
/*   class CirMgr member functions for circuit construction   */
/**************************************************************/
CirGate*
CirMgr::getGate(unsigned gid) const
{
	if (gid > (_maxVar + _outNum)) return 0;
	if (_totList[gid]) return _totList[gid];
	else return 0;
}

bool
CirMgr::readCircuit(const string& fileName)
{
   ifstream cirFile(fileName.c_str());
	if(!cirFile) {
		cerr << "Cannot open design \"" << fileName << "\"!!" << endl;
		return false;
	}
	
	lineNo = 0; colNo = 0; errMsg = ""; errInt = 0; // initialize
	
	/*******************/
	/*   Read header   */
	/*******************/
	string strBuf;
	vector<string> fileText;
   if (getline(cirFile, strBuf)) {
   	vector<string> tokens;
   	mulTokens(strBuf, tokens);
   	int intTest = 0;
   	/* Header error handling */
   	if (strBuf.size() == 0) { errMsg = "aag"; return parseError(MISSING_IDENTIFIER); }
   	if (strBuf.size() != 0 && strBuf[0] == ' ') return parseError(EXTRA_SPACE);
   	else if (strBuf.size() != 0 && strBuf[0] == '\t') { errInt = 9; return parseError(ILLEGAL_WSPACE); }
   	else if (tokens.size() != 0 && tokens[0] != "aag" && !myStr2Int(tokens[0].substr(3, 1), intTest)) {
   		errMsg = tokens[0];
   		return parseError(ILLEGAL_IDENTIFIER);
   	}
   	else if (strBuf.size() >= tokens[0].size() + 2 && strBuf.substr(tokens[0].size() + 1, 1) == " ") {
   		colNo = tokens[0].size() + 1;
   		return parseError(EXTRA_SPACE);
   	}
   	else if (strBuf.size() >= 4 && strBuf[0] == 'a' && strBuf[1] == 'a' && strBuf[2] == 'g' && strBuf[3] != ' ') {
   		colNo = 3;
   		return parseError(MISSING_SPACE);
   	}
   	else if (strBuf.size() >= tokens[0].size() + 2 && strBuf.substr(tokens[0].size() + 1, 1) == "\t") {
   		colNo = tokens[0].size() + 1;
   		errInt = 9; 
   		return parseError(ILLEGAL_WSPACE);
   	}
   	else if (tokens.size() >= 1 && tokens.size() <= 6) {
   		if (tokens.size() == 1) {
				colNo = tokens[0].size() + 1;
				if (tokens[0] == "aag" && strBuf.size() == 3) { colNo = tokens[0].size(); }
				errMsg = "number of vars";
				return parseError(MISSING_NUM);
   		}
   		for (size_t i = 1; i < tokens.size(); ++i) {
   			string tempStr = " " + tokens[i] + " ";
   			
   			if (i != tokens.size() - 1 && strBuf.size() >= strBuf.find(tempStr) + tempStr.size()
   				 && strBuf.substr(strBuf.find(tempStr) + tempStr.size() - 1, 1) != " ") {
   				colNo = strBuf.find(tempStr) + tempStr.size() - 1;
   				return parseError(MISSING_SPACE);
   			}
   			else if (i != tokens.size() - 1 && tokens[i].find_first_of('\t') != string::npos) {
   				colNo = strBuf.find(tokens[i]) + 1;
   				return parseError(MISSING_SPACE);
   			}
   			else if (i != tokens.size() - 1 && strBuf.size() >= strBuf.find(tempStr) + tempStr.size() + 1
   						&& strBuf.substr(strBuf.find(tempStr) + tempStr.size(), 1) == " ") {
   				colNo = strBuf.find(tempStr) + tempStr.size();
   				return parseError(EXTRA_SPACE);
   			}
   			
   			if (!myStr2Int(tokens[i], intTest)) {
   				switch (i) { // detect if number is valid
   					case 1:
   						errMsg = "number of vars(" + tokens[1] + ")";
   						colNo  = strBuf.find(tokens[1]) + tokens[1].size() + 1;
   						return parseError(ILLEGAL_NUM);
   					case 2:
   						errMsg = "number of PIs(" + tokens[2] + ")";
   						colNo  = strBuf.find(tokens[2]) + tokens[2].size() + 1;
   						return parseError(ILLEGAL_NUM);
   					case 3:
   						errMsg = "number of latches(" + tokens[3] + ")";
   						colNo  = strBuf.find(tokens[3]) + tokens[3].size() + 1;
   						return parseError(ILLEGAL_NUM);
   					case 4:
   						errMsg = "number of POs(" + tokens[4] + ")";
   						colNo  = strBuf.find(tokens[4]) + tokens[4].size() + 1;
   						return parseError(ILLEGAL_NUM);
   					case 5:
   						if (tokens[5].find_first_of('\t') != string::npos) {
								string tempStr = " " + tokens[5];
								colNo = strBuf.find(tempStr) + 2;
								return parseError(MISSING_NEWLINE);
							}
   						errMsg = "number of AIGs(" + tokens[5] + ")";
   						colNo  = strBuf.find(tokens[5]) + tokens[5].size() + 1;
   						return parseError(ILLEGAL_NUM);
   				}
   			}
   			
   			if (tokens.size() >= 6 && strBuf.substr(strBuf.size() - 1, 1) == " ") {
   				colNo  = strBuf.size() - 1;
					return parseError(MISSING_NEWLINE);
   			}
   		}
   		// detect if number is missing
			switch (tokens.size()) {
				case 1:
					errMsg = "number of vars";
					colNo = strBuf.size();
					return parseError(MISSING_NUM);
				case 2:
					errMsg = "number of PIs";
					colNo = strBuf.size();
					return parseError(MISSING_NUM);
				case 3:
					errMsg = "number of latches";
					colNo = strBuf.size();
					return parseError(MISSING_NUM);
				case 4:
					errMsg = "number of POs";
					colNo = strBuf.size();
					return parseError(MISSING_NUM);
				case 5:
					errMsg = "number of AIGs";
					colNo = strBuf.size();
					return parseError(MISSING_NUM);
			}
   	}
   	else if (tokens.size() > 6) {
   		string tempStr = " " + tokens[5] + " ";
   		colNo = strBuf.find(tempStr) + tempStr.size() - 1;
			return parseError(MISSING_NEWLINE);
   	}
   	
   	++lineNo;
		int maxVar, inNum, outNum, latchNum, andGateNum;
		if (!myStr2Int(tokens[1], maxVar))     return false;
		if (!myStr2Int(tokens[2], inNum))      return false;
		if (!myStr2Int(tokens[3], latchNum))   return false;
		if (!myStr2Int(tokens[4], outNum))     return false;
		if (!myStr2Int(tokens[5], andGateNum)) return false;
		_maxVar 		= (unsigned)maxVar;
		_inNum  		= (unsigned)inNum;
		_outNum 		= (unsigned)outNum;
		_latchNum   = (unsigned)latchNum;
		_andGateNum = (unsigned)andGateNum;
		_totList.resize(_maxVar + _outNum + 1);
		if (_latchNum != 0) {
			--lineNo; errMsg = "latches";
			return parseError(ILLEGAL_NUM);
		}
		else if (_inNum + _andGateNum + _latchNum > _maxVar) {
			--lineNo; errInt = _maxVar; errMsg = "Num of variables";
			return parseError(NUM_TOO_SMALL);
		}
		fileText.push_back(strBuf);
   }
   else { errMsg = "aag"; return parseError(MISSING_IDENTIFIER); }
   
	/**************************/
	/*   Read constant gate   */
	/**************************/
   CirGate* constGate = new CirConstGate;
   _totList[0] = constGate;
   
	/****************/
	/*   Read PIs   */
	/****************/
   for (unsigned i = 0; i < _inNum; ++i) {
   	if (getline(cirFile, strBuf)) {
   		/* PIs error handling */
   		vector<string> tokens;
   	   mulTokens(strBuf, tokens);
   	   if (strBuf.size() == 0 || tokens.size() == 0) { 
   	   	errMsg = "PI literal ID";
   	   	return parseError(MISSING_NUM);
   	   }
   		if (strBuf.size() != 0 && strBuf[0] == ' ') return parseError(EXTRA_SPACE);
	   	else if (strBuf.size() != 0 && strBuf[0] == '\t') { errInt = 9; return parseError(ILLEGAL_WSPACE); }
	   	if (strBuf.size() != 0 && strBuf.substr(strBuf.size() - 1, 1) == " ") {
				colNo  = strBuf.size() - 1;
				return parseError(MISSING_NEWLINE);
			}
			if (tokens.size() == 1 && tokens[0].find_first_of('\t') != string::npos) {
				string tempStr = " " + tokens[0];
				colNo = strBuf.find(tempStr) + 2;
				return parseError(MISSING_NEWLINE);
			}
	   	if (tokens.size() > 1) {
				string tempStr = " " + tokens[1] + " ";
				colNo = strBuf.find(tempStr) + tempStr.size() - 1;
				return parseError(MISSING_NEWLINE);
			}
   		++lineNo;
   		CirGate* piGate = new CirPIGate;
   		int inputID;
   		if (myStr2Int(tokens[0], inputID)) {
   			if (inputID < 0) {
   				--lineNo;
   				errMsg = "PI literal ID(" + tokens[0] + ")";
   				return parseError(ILLEGAL_NUM);
   			}
   			if (inputID == 0 || inputID == 1) {
   				--lineNo;
   				errInt = inputID;
   				return parseError(REDEF_CONST);
   			}
   			if (inputID % 2 == 1) {
   				--lineNo;
   				errMsg = "PI"; errInt = inputID;
   				return parseError(CANNOT_INVERTED);
   			}
   			int tempInt = inputID;
   			inputID /= 2;
   			if ((unsigned)inputID > _maxVar) {
   				--lineNo; errInt = tempInt;
   				return parseError(MAX_LIT_ID);
   			}
   			piGate->setGateID((unsigned)inputID);
   			piGate->setLineNo(lineNo);
   			if (_totList[inputID] != 0) {
					--lineNo; errInt = tempInt;
					errGate = _totList[inputID];
					return parseError(REDEF_GATE);
				}
   		}
   		_piList.push_back(piGate);
   		_totList[inputID] = piGate;
   		fileText.push_back(strBuf);
   	}
   	else { errMsg = "PI"; return parseError(MISSING_DEF); }
   }
   
	/****************/
	/*   Read POs   */
	/****************/
   for (unsigned i = 0; i < _outNum; ++i) {
   	if (getline(cirFile, strBuf)) {
   		/* POs error handling */
   		vector<string> tokens;
   	   mulTokens(strBuf, tokens);
   	   if (strBuf.size() == 0 || tokens.size() == 0) { 
   	   	errMsg = "PO literal ID";
   	   	return parseError(MISSING_NUM);
   	   }
   		if (strBuf.size() != 0 && strBuf[0] == ' ') return parseError(EXTRA_SPACE);
	   	else if (strBuf.size() != 0 && strBuf[0] == '\t') { errInt = 9; return parseError(ILLEGAL_WSPACE); }
	   	if (strBuf.size() != 0 && strBuf.substr(strBuf.size() - 1, 1) == " ") {
				colNo  = strBuf.size() - 1;
				return parseError(MISSING_NEWLINE);
			}
			if (tokens.size() == 1 && tokens[0].find_first_of('\t') != string::npos) {
				string tempStr = " " + tokens[0];
				colNo = strBuf.find(tempStr) + 2;
				return parseError(MISSING_NEWLINE);
			}
	   	if (tokens.size() > 1) {
				string tempStr = " " + tokens[1] + " ";
				colNo = strBuf.find(tempStr) + tempStr.size() - 1;
				return parseError(MISSING_NEWLINE);
			}
   		++lineNo;
   		CirGate* poGate = new CirPOGate;
   		int outputID;
   		if (myStr2Int(tokens[0], outputID)) {
   			if (outputID < 0) {
   				--lineNo;
   				errMsg = "PO literal ID(" + tokens[0] + ")";
   				return parseError(ILLEGAL_NUM);
   			}
   			int tempInt = outputID;
   			outputID /= 2;
   			if ((unsigned)outputID > _maxVar) {
   				--lineNo; errInt = tempInt;
   				return parseError(MAX_LIT_ID);
   			}
   			poGate->setGateID(_maxVar + i + 1);
   			poGate->setLineNo(lineNo);
   		}
   		else {
   			--lineNo; errMsg = "PO literal ID(" + tokens[0] + ")";
				return parseError(ILLEGAL_NUM);
   		}
   		_poList.push_back(poGate);
   		_totList[_maxVar + i + 1] = poGate;
   		fileText.push_back(strBuf);
   	}
   	else { errMsg = "PO"; return parseError(MISSING_DEF); }
   }
   
	/**********************/
	/*   Read AIG gates   */
	/**********************/
   for (unsigned i = 0; i < _andGateNum; ++i) {
   	if (getline(cirFile, strBuf)) {
   		vector<string> tokens;
   		mulTokens(strBuf, tokens);
   		/* AIGs error handling */
   		if (strBuf.size() == 0 || tokens.size() == 0) { 
   	   	errMsg = "AIG literal ID";
   	   	return parseError(MISSING_NUM);
   	   }
   		if (strBuf.size() != 0 && strBuf[0] == ' ') return parseError(EXTRA_SPACE);
	   	else if (strBuf.size() != 0 && strBuf[0] == '\t') { errInt = 9; return parseError(ILLEGAL_WSPACE); }
	   	int aigID;
   		if(!myStr2Int(tokens[0], aigID) || aigID < 0) {
				errMsg = "AIG gate literal ID(" + tokens[0] + ")";
				return parseError(ILLEGAL_NUM);
   		}
	   	if (strBuf.size() == 1) { 
	   		colNo = 1;
   			return parseError(MISSING_SPACE); 
   		}
   		int ttempInt = 0;
   		myStr2Int(tokens[0], ttempInt);
   		int tttempInt = ttempInt;
   		ttempInt /= 2;
			if ((unsigned)ttempInt > _maxVar) {
				errInt = tttempInt;
				return parseError(MAX_LIT_ID);
			}
   		for (size_t i = 1; i < tokens.size(); ++i) {
				string tempStr = " " + tokens[i] + " ";
   			
   			if (i != 2 && strBuf.size() >= strBuf.find(tempStr) + tempStr.size()
   				 && strBuf.substr(strBuf.find(tempStr) + tempStr.size() - 1, 1) != " ") {
   				colNo = strBuf.find(tempStr) + tempStr.size() - 1;
   				return parseError(MISSING_SPACE);
   			}
   			else if (i != 2 && tokens[i].find_first_of('\t') != string::npos) {
   				colNo = strBuf.find(tokens[i]) + 1;
   				return parseError(MISSING_SPACE);
   			}
   			else if (i != 2 && strBuf.size() >= strBuf.find(tempStr) + tempStr.size() + 1
   						&& strBuf.substr(strBuf.find(tempStr) + tempStr.size(), 1) == " ") {
   				colNo = strBuf.find(tempStr) + tempStr.size();
   				return parseError(EXTRA_SPACE);
   			}
   			int tempInt = 0;
   			if (!myStr2Int(tokens[i], tempInt) || tempInt < 0) {
   				errMsg = "AIG input literal ID(" + tokens[i] + ")";
					return parseError(ILLEGAL_NUM);
   			}
   			int tempIInt = tempInt;
   			tempInt /= 2;
   			if ((unsigned)tempInt > _maxVar) {
   				errInt = tempIInt;
   				if (i == 2) { tempStr = " " + tokens[i]; }
   				colNo = strBuf.find(tempStr) + 1;
   				return parseError(MAX_LIT_ID);
   			}
   		}
	   	if (strBuf.size() != 0 && strBuf.substr(strBuf.size() - 1, 1) == " ") {
				colNo  = strBuf.size() - 1;
				return parseError(MISSING_NEWLINE);
			}
			if (tokens.size() == 3 && tokens[2].find_first_of('\t') != string::npos) {
				string tempStr = " " + tokens[2];
				colNo = strBuf.find(tempStr) + 2;
				return parseError(MISSING_NEWLINE);
			}
	   	if (tokens.size() > 3) {
				string tempStr = " " + tokens[2] + " ";
				colNo = strBuf.find(tempStr) + tempStr.size() - 1;
				return parseError(MISSING_NEWLINE);
			}
			
   		++lineNo;
   		CirGate* aigGate = new CirAIGGate;
   		int tempInt = aigID;
			if (aigID == 0 || aigID == 1) {
				--lineNo;
				errInt = aigID;
				return parseError(REDEF_CONST);
			}
			if (aigID % 2 == 1) {
				--lineNo;
				errMsg = "AIG gate"; errInt = aigID;
				return parseError(CANNOT_INVERTED);
			}
			aigID /= 2;
			aigGate->setGateID((unsigned)aigID);
			aigGate->setLineNo(lineNo);
			if (_totList[aigID] != 0 && _totList[aigID]->getTypeStr() != "AIG") {
				--lineNo; errInt = tempInt;
				errGate = _totList[aigID];
				return parseError(REDEF_GATE);
			}
   		else if (_totList[aigID] != 0 && _totList[aigID]->getLineNo() > 1 + _inNum + _outNum) {
   			--lineNo; errInt = tempInt;
				errGate = _totList[aigID];
				return parseError(REDEF_GATE);
   		}
   		++_aigNum;
   		_totList[aigID] = aigGate;
   		fileText.push_back(strBuf);
   	}
   	else { errMsg = "AIG"; return parseError(MISSING_DEF); }
   }
   
	/****************************/
	/*   Read symbol sections   */
	/****************************/
   while (getline(cirFile, strBuf) && strBuf[0] != 'c') {
   	/* Symbol error handling */
   	int num = 0;
   	vector<string> tokens; 
   	mulTokens(strBuf, tokens);
   	if (strBuf.size() == 0) { errMsg = '\0'; return parseError(ILLEGAL_SYMBOL_TYPE); }
   	if (strBuf.size() != 0 && strBuf[0] == ' ') return parseError(EXTRA_SPACE);
	   else if (strBuf.size() != 0 && strBuf[0] == '\t') { errInt = 9; return parseError(ILLEGAL_WSPACE); }
   	if (strBuf[0] != 'i' && strBuf[0] != 'o') {
   		errMsg = tokens[0].substr(0, 1);
   		return parseError(ILLEGAL_SYMBOL_TYPE);
   	}
   	else if ((strBuf[0] == 'i' || strBuf[0] == 'o') && (strBuf[1] == ' ' || strBuf[1] == '\t')) {
   		++colNo;
   		if (strBuf[1] == ' ') return parseError(EXTRA_SPACE);
   		if (strBuf[1] == '\t') { errInt = 9; return parseError(ILLEGAL_WSPACE); }
   	}
   	colNo = 0;
   	if (!myStr2Int(tokens[0].substr(1), errInt)) {
   		if (strBuf.size() >= tokens[0].size() && strBuf.substr(tokens[0].size() - 1, 1) == "\t") {
				colNo = tokens[0].size() - 1;
				return parseError(MISSING_SPACE);
			}
   		errMsg = "symbol index(" + tokens[0].substr(1) + ")";
         return parseError(ILLEGAL_NUM);
   	}
   	else myStr2Int(tokens[0].substr(1), num);
   		
   	if (strBuf[0] == 'i' && (unsigned)errInt > _inNum - 1) {
   		errMsg = "PI index";
   		return parseError(NUM_TOO_BIG);
   	}
   	else if (strBuf[0] == 'i' && (unsigned)errInt < 0) {
   		errMsg = "symbol index";
   		return parseError(ILLEGAL_NUM);
   	}
   	else if (strBuf[0] == 'o' && (unsigned)errInt > _outNum - 1) {
   		errMsg = "PO index";
   		return parseError(NUM_TOO_BIG);
   	}
   	else if (strBuf[0] == 'o' && (unsigned)errInt < 0) {
   		errMsg = "symbol index";
   		return parseError(ILLEGAL_NUM);
   	}
   	
   	// Symbol set
   	if (strBuf[0] == 'i') {
   		if (strBuf.size() <= tokens[0].size() + 1) {
   			errMsg = "symbolic name";
				return parseError(MISSING_IDENTIFIER);
   		}
   		if (_piList[(unsigned)num]->getSymbol() == "") {
				if (strBuf.size() > tokens[0].size() + 1 && strBuf.substr(tokens[0].size(), 1) == " ") {
					string tempStr = strBuf.substr(tokens[0].size() + 1);
					for (size_t i = 0; i < tempStr.size(); ++i) {
						if (0 <= int(tempStr[i]) && int(tempStr[i]) <= 31) {
							colNo = strBuf.find(tempStr[i]); errInt = int(tempStr[i]);
							return parseError(ILLEGAL_SYMBOL_NAME);
						}
					}
					_piList[(unsigned)num]->setSymbol(strBuf.substr(tokens[0].size() + 1));
				}
				else {
					errMsg = "symbolic name";
					return parseError(MISSING_IDENTIFIER);
				}
			}
			else {
				errMsg = strBuf[0]; errInt = num;
				return parseError(REDEF_SYMBOLIC_NAME);
			}
   	}
   	else if (strBuf[0] == 'o') {
   		if (strBuf.size() <= tokens[0].size() + 1) {
   			errMsg = "symbolic name";
				return parseError(MISSING_IDENTIFIER);
   		}
   		if (_poList[(unsigned)num]->getSymbol() == "") {
				if (strBuf.size() > tokens[0].size() + 1 && strBuf.substr(tokens[0].size(), 1) == " ") {
					string tempStr = strBuf.substr(tokens[0].size() + 1);
					for (size_t i = 0; i < tempStr.size(); ++i) {
						if (0 <= int(tempStr[i]) && int(tempStr[i]) <= 31) {
							colNo = strBuf.find(tempStr[i]); errInt = int(tempStr[i]);
							return parseError(ILLEGAL_SYMBOL_NAME);
						}
					}
					_poList[(unsigned)num]->setSymbol(strBuf.substr(tokens[0].size() + 1));
				}
				else {
					errMsg = "symbolic name";
					return parseError(MISSING_IDENTIFIER);
				}
			}
			else { errMsg = strBuf[0]; errInt = num; return parseError(REDEF_SYMBOLIC_NAME); }
   	}
   	
   	++lineNo;
   }
   if (strBuf.size() != 0 && strBuf[0] == 'c') {
   	if (strBuf.size() > 1) {
   		colNo = 1;
   		return parseError(MISSING_NEWLINE);
   	}
   }
   
   cirFile.close(); // close the file
   
	/************************/
	/*   Make connections   */
	/************************/
   lineNo = _inNum + 1;
   
   for (unsigned i = _inNum + 1; i < 1 + _inNum + _outNum; ++i) { // set input of POs
		strBuf = fileText[i]; ++lineNo;
		unsigned outID = _maxVar - _inNum + i; int outInID = 0;
		vector<string> tokens; mulTokens(strBuf, tokens); myStr2Int(tokens[0], outInID);
		bool isInv0 = false; bool isInv1 = false;
   	if (outInID % 2 == 1) { isInv0 = true; }
   	outInID /= 2;
   	if (_totList[(unsigned)outInID]) {
			_totList[outID]->setInput(_totList[(unsigned)outInID], isInv0, 0, isInv1);
			_totList[(unsigned)outInID]->addOutput(_totList[outID]);
		}
		else {
			CirGate* undefGate = new CirUndefGate;
			undefGate->setGateID(outInID); undefGate->setLineNo(0);
			_undefList.push_back(undefGate); _totList[outInID] = undefGate;
			_totList[outID]->setInput(_totList[(unsigned)outInID], isInv0, 0, isInv1);
			_totList[(unsigned)outInID]->addOutput(_totList[outID]);
		}
   }
   
   for (unsigned i = _inNum + 1 + _outNum; i < 1 + _inNum + _outNum + _andGateNum; ++i) { 
   	// set input of AIG gates, also set floating gates
   	strBuf = fileText[i];
		++lineNo;
		vector<string> tokens; 
   	mulTokens(strBuf, tokens); // get tokens
   	int aigID, aigIn0, aigIn1 = 0;
   	if (tokens.size() >= 3) {
			myStr2Int(tokens[0], aigID);
			myStr2Int(tokens[1], aigIn0);
			myStr2Int(tokens[2], aigIn1);
   	}
   	else return false;
   	
   	bool isInv0 = false; bool isInv1 = false;
   	if (aigIn0 % 2 == 1) { isInv0 = true; }
   	if (aigIn1 % 2 == 1) { isInv1 = true; }
   	aigID /= 2; aigIn0 /= 2; aigIn1 /= 2; // divide 2 to get ID
   	if (aigID >= 0 && aigIn0 >= 0 && aigIn1 >= 0) {
   		if (_totList[(unsigned)aigIn0] == 0 && aigIn0 != 0) { // If not defined before
   			CirGate* undefGate = new CirUndefGate;
   			undefGate->setGateID(aigIn0); undefGate->setLineNo(0);
   			_undefList.push_back(undefGate); 					
   			_totList[aigIn0] = undefGate;
   		}
   		if (_totList[(unsigned)aigIn1] == 0 && aigIn1 != 0) { // If not defined before
   			CirGate* undefGate = new CirUndefGate; 
   			undefGate->setGateID(aigIn1); undefGate->setLineNo(0);
   			_undefList.push_back(undefGate);
				_totList[aigIn1] = undefGate;
   		}
			_totList[(unsigned)aigIn0]->addOutput(_totList[(unsigned)aigID]);
			_totList[(unsigned)aigIn1]->addOutput(_totList[(unsigned)aigID]);
			_totList[(unsigned)aigID]->setInput(_totList[(unsigned)aigIn0], isInv0, _totList[(unsigned)aigIn1], isInv1);
		}
		else return false;
   }
   fileText.resize(0);
	return true;
}

/**********************************************************/
/*   class CirMgr member functions for circuit printing   */
/**********************************************************/
void
CirMgr::printSummary() const
{
	unsigned sum = _piList.size() + _poList.size() + _aigNum;
	cout << endl;
	cout << "Circuit Statistics" << endl
		  << "==================" << endl
		  << "  PI " << setw(11)  << right << _piList.size()  << endl
		  << "  PO " << setw(11)  << right << _poList.size()  << endl
		  << "  AIG" << setw(11)  << right << _aigNum 			<< endl
		  << "------------------" << endl
		  << "  Total" << setw(9) << right << sum             << endl;
}

void
CirMgr::printNetlist()
{
	if (_dfsList.size() == 0)
		cirDFSearch();
	cout << endl;
	for (unsigned i = 0; i < _dfsList.size(); ++i) {
		cout << "[" << i << "] ";
		cout << setw(4) << left << _dfsList[i]->getTypeStr();
		cout << _dfsList[i]->getGateID();
		if (_dfsList[i]->getTypeStr() == "PO") {
			if (_dfsList[i]->getisInv0() && _dfsList[i]->getInput0()->getTypeStr() == "UNDEF")
				cout << " *!" << _dfsList[i]->getInput0()->getGateID();
			else if (_dfsList[i]->getInput0() && _dfsList[i]->getInput0()->getTypeStr() == "UNDEF")
				cout << " *" << _dfsList[i]->getInput0()->getGateID();
			else if (_dfsList[i]->getisInv0()) { cout << " !" << _dfsList[i]->getInput0()->getGateID(); }
			else if (_dfsList[i]->getInput0()) { cout << " " << _dfsList[i]->getInput0()->getGateID(); }
			if (_dfsList[i]->getSymbol() != "")
				cout << " " << "(" << _dfsList[i]->getSymbol() << ")";
		}
		else if (_dfsList[i]->getTypeStr() == "AIG") {
			// Input 0
			if (_dfsList[i]->getisInv0() && _dfsList[i]->getInput0()->getTypeStr() == "UNDEF")
				cout << " *!" << _dfsList[i]->getInput0()->getGateID();
			else if (_dfsList[i]->getInput0() && _dfsList[i]->getInput0()->getTypeStr() == "UNDEF")
				cout << " *" << _dfsList[i]->getInput0()->getGateID();
			else if (_dfsList[i]->getisInv0()) { cout << " !" << _dfsList[i]->getInput0()->getGateID(); }
			else if (_dfsList[i]->getInput0()) { cout << " " << _dfsList[i]->getInput0()->getGateID(); }
			// Input 1
			if (_dfsList[i]->getisInv1() && _dfsList[i]->getInput1()->getTypeStr() == "UNDEF")
				cout << " *!" << _dfsList[i]->getInput1()->getGateID();
			else if (_dfsList[i]->getInput1() && _dfsList[i]->getInput1()->getTypeStr() == "UNDEF")
				cout << " *" << _dfsList[i]->getInput1()->getGateID();
			else if (_dfsList[i]->getisInv1()) { cout << " !" << _dfsList[i]->getInput1()->getGateID(); }
			else if (_dfsList[i]->getInput1()) { cout << " " << _dfsList[i]->getInput1()->getGateID(); }
		}
		else if (_dfsList[i]->getTypeStr() == "PI" && _dfsList[i]->getSymbol() != "")
			cout << " (" << _dfsList[i]->getSymbol() << ")";
		cout << endl;
	}
}

void
CirMgr::printPIs() const
{
   cout << "PIs of the circuit:";
   for (size_t i = 0; i < _piList.size(); ++i)
   	cout << " " << _piList[i]->getGateID();
   cout << endl;
}

void
CirMgr::printPOs() const
{
   cout << "POs of the circuit:";
   for (size_t i = 0; i < _poList.size(); ++i)
   	cout << " " << _poList[i]->getGateID();
   cout << endl;
}

void
CirMgr::printFloatGates() const
{
	// Print out floating fanin(s)
	if (_undefList.size()) {
		cout << "Gates with floating fanin(s):";
		IdList usedID; bool notUsed = true;
		for (unsigned i = 0; i < _undefList.size(); ++i)
			for (unsigned j = 0; j < _undefList[i]->getOutput().size(); ++j) {
				for (unsigned k = 0; k < usedID.size(); ++k)
					if (_undefList[i]->getOutput()[j]->getGateID() == usedID[k]) { notUsed = false; break; }
				if (notUsed) usedID.push_back(_undefList[i]->getOutput()[j]->getGateID());
				notUsed = true;
			}
		sort(usedID.begin(), usedID.end());
		for (unsigned i = 0; i < usedID.size(); ++i)
			cout << " " << usedID[i];
		cout << endl;
	}
	// Print gates without fanout(s)
	bool defNotUse = false; size_t begin = 0;
	for (unsigned i = 0; i < _totList.size(); ++i)
		if (_totList[i] && _totList[i]->getTypeStr() != "PO" && !_totList[i]->getOutput().size() && _totList[i]->getGateID()) {
			defNotUse = true; begin = i; break;
		}
	if (defNotUse) {
		cout << "Gates defined but not used  :";
		for (unsigned i = begin; i < _totList.size(); ++i)
			if (_totList[i] && _totList[i]->getTypeStr() != "PO" && !_totList[i]->getOutput().size() && _totList[i]->getGateID())
				cout << " " << _totList[i]->getGateID();
		cout << endl;
	}
}

void
CirMgr::printFECPairs() const
{
	size_t num = 0;
	for (size_t i = 0; i < _fecGroups.size(); ++i) {
		if (_fecGroups[i][0] != -1 && _fecGroups[i].size() >= 2) {
			IdList tempList;
			for (size_t j = 0; j < _fecGroups[i].size(); ++j)
				if (_fecGroups[i][j] != -1) tempList.push_back((size_t)_fecGroups[i][j]);
			if (tempList.size() >= 2) {
				cout << "[" << num << "] " << tempList[0];
				CirGate* leadGate = _totList[tempList[0]];
				size_t leadVal = leadGate->getGateValue();
				for (size_t j = 1; j < tempList.size(); ++j) {
					if (leadVal == _totList[tempList[j]]->getGateValue()) cout << " " << tempList[j];
					else cout << " !" << tempList[j];
				}
				++num;
				cout << endl;
			}
		}
	}
}

void
CirMgr::writeAag(ostream& outfile)
{
	if (_dfsList.size() == 0)
		cirDFSearch();
	unsigned aigDFSnum = 0;
	vector<string> inputSymbol;
	vector<string> outputSymbol;
	for (unsigned i = 0; i < _dfsList.size(); ++i)
		if (_dfsList[i]->getTypeStr() == "AIG") ++aigDFSnum;
	outfile << "aag " << _maxVar << " " << _inNum << " " << _latchNum << " " << _outNum << " " << aigDFSnum << endl;
	for (unsigned i = 0; i < _piList.size(); ++i) {
		outfile << _piList[i]->getGateID() * 2 << endl;
		if (_piList[i]->getSymbol() != "")
			inputSymbol.push_back(_piList[i]->getSymbol());
	}
	for (unsigned i = 0; i < _poList.size(); ++i) {
		if (_poList[i]->getisInv0()) outfile << _poList[i]->getInput0()->getGateID() * 2 + 1 << endl;
		else outfile << _poList[i]->getInput0()->getGateID() * 2 << endl;
		if (_poList[i]->getSymbol() != "")
			outputSymbol.push_back(_poList[i]->getSymbol());
	}
	for (unsigned i = 0; i < _dfsList.size(); ++i) {
		if (_dfsList[i]->getTypeStr() == "AIG") { 
			outfile << _dfsList[i]->getGateID() * 2;
			if (_dfsList[i]->getisInv0()) outfile << " " << _dfsList[i]->getInput0()->getGateID() * 2 + 1;
			else outfile << " " << _dfsList[i]->getInput0()->getGateID() * 2;
			if (_dfsList[i]->getisInv1()) outfile << " " << _dfsList[i]->getInput1()->getGateID() * 2 + 1;
			else outfile << " " << _dfsList[i]->getInput1()->getGateID() * 2;
			outfile << endl;
		}
	}
	if (inputSymbol.size() != 0) {
		for (size_t i = 0; i < inputSymbol.size(); ++i)
			outfile << "i" << i << " " << inputSymbol[i] << endl;
	}
	if (outputSymbol.size() != 0) {
		for (size_t i = 0; i < outputSymbol.size(); ++i)
			outfile << "o" << i << " " << outputSymbol[i] << endl;
	}
	outfile << "c" << endl;
	outfile << "AAG output by Da-Min (Dan) Huang" << endl;
}

/***********************************************************/
/*   class CirMgr member functions for circuit searching   */
/***********************************************************/
void
CirMgr::cirDFSearch()
{
	CirGate::setGlobalRef();
	if (_poList.size() != 0)
		for (unsigned i = 0; i < _poList.size(); ++i)
			_poList[i]->gateDFSearch();
	else return;
}

/******************************************************/
/*   class CirMgr member functions for gate merging   */
/******************************************************/
void
CirMgr::gateMerge(CirGate* inGate, CirGate* thisGate, bool valInv)
{
	/*   inGate --> thisGate(removed) --> outList   */
	
	/* Clean inputs of thisGate */
	if (thisGate->getInput0()) { thisGate->getInput0()->removeOutput(thisGate); }
	if (thisGate->getInput1()) { thisGate->getInput1()->removeOutput(thisGate); }
	/* Clean links between inGate and thisGate */
	inGate->removeOutput(thisGate); inGate->removeOutput(thisGate);
	/* Merge */
	GateList outList = thisGate->getOutput();
	for (size_t i = 0; i < outList.size(); ++i) {
		/* Add outputs of thisGate to inGate */
		inGate->addOutput(outList[i]);
		/* Re-set inputs of outputs of thisGate */
		bool outIn0IsThis = (outList[i]->getInput0() == thisGate); bool thisIn0IsIn = (thisGate->getInput0() == inGate);
		bool outIn1IsThis = (outList[i]->getInput1() == thisGate); bool thisIn1IsIn = (thisGate->getInput1() == inGate);
		bool mergeBool0 = (outList[i]->getisInv0() != thisGate->getisInv0());
		bool mergeBool1 = (outList[i]->getisInv0() != thisGate->getisInv1());
		bool mergeBool2 = (outList[i]->getisInv1() != thisGate->getisInv0());
		bool mergeBool3 = (outList[i]->getisInv1() != thisGate->getisInv1());
		if (outIn0IsThis && !outIn1IsThis) {
			if (thisIn0IsIn && !thisIn1IsIn)
				outList[i]->setInput(inGate, mergeBool0, outList[i]->getInput1(), outList[i]->getisInv1());
			else if (!thisIn0IsIn && thisIn1IsIn)
				outList[i]->setInput(inGate, mergeBool1, outList[i]->getInput1(), outList[i]->getisInv1());
			else if (thisIn0IsIn && thisIn1IsIn) {
				if (thisGate->getisInv0() == thisGate->getisInv1())
					outList[i]->setInput(inGate, mergeBool0 && mergeBool1, outList[i]->getInput1(), outList[i]->getisInv1());
				else outList[i]->setInput(inGate, outList[i]->getisInv0(), outList[i]->getInput1(), outList[i]->getisInv1());
			}
			else {
				if (!valInv) outList[i]->setInput(inGate, outList[i]->getisInv0(), outList[i]->getInput1(), outList[i]->getisInv1());
				else outList[i]->setInput(inGate, !outList[i]->getisInv0(), outList[i]->getInput1(), outList[i]->getisInv1());
			}
		}
		else if (!outIn0IsThis && outIn1IsThis) {
			if (thisIn0IsIn && !thisIn1IsIn)
				outList[i]->setInput(outList[i]->getInput0(), outList[i]->getisInv0(), inGate, mergeBool2);
			else if (!thisIn0IsIn && thisIn1IsIn)
				outList[i]->setInput(outList[i]->getInput0(), outList[i]->getisInv0(), inGate, mergeBool3);
			else if (thisIn0IsIn && thisIn1IsIn) {
				if (thisGate->getisInv0() == thisGate->getisInv1())
					outList[i]->setInput(outList[i]->getInput0(), outList[i]->getisInv0(), inGate, mergeBool2 && mergeBool3);
				else outList[i]->setInput(outList[i]->getInput0(), outList[i]->getisInv0(), inGate, outList[i]->getisInv1());
			}
			else {
				if (!valInv) outList[i]->setInput(outList[i]->getInput0(), outList[i]->getisInv0(), inGate, outList[i]->getisInv1());
				else outList[i]->setInput(outList[i]->getInput0(), outList[i]->getisInv0(), inGate, !outList[i]->getisInv1());
			}
		}
		else if (outIn0IsThis && outIn1IsThis) {
			if (thisIn0IsIn && !thisIn1IsIn)
				outList[i]->setInput(inGate, mergeBool0, inGate, mergeBool2);
			else if (!thisIn0IsIn && thisIn1IsIn)
				outList[i]->setInput(inGate, mergeBool1, inGate, mergeBool3);
			else if (thisIn0IsIn && thisIn1IsIn) {
				if (thisGate->getisInv0() == thisGate->getisInv1())
					outList[i]->setInput(inGate, mergeBool0, inGate, mergeBool3);
				else outList[i]->setInput(inGate, outList[i]->getisInv0(), inGate, outList[i]->getisInv1());
			}
			else {
				if (!valInv) outList[i]->setInput(inGate, outList[i]->getisInv0(), inGate, outList[i]->getisInv1());
				else outList[i]->setInput(inGate, !outList[i]->getisInv0(), inGate, !outList[i]->getisInv1());
			}
		}
	}
	/* Clean outputs of thisGate */
	thisGate->clearOutput();
}

/*************************************/
/*   class CirMgr Helper functions   */
/*************************************/
bool
CirMgr::mulTokens(const string& option, vector<string>& tokens) const
{
	string token; size_t n = myStrGetTok(option, token);
   while (token.size()) {
      tokens.push_back(token);
      n = myStrGetTok(option, token, n);
   }
   return true;
}