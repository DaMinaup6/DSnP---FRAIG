/****************************************************************************
  FileName     [ cirMgr.h ]
  PackageName  [ cir ]
  Synopsis     [ Define circuit manager ]
  Author       [ Chung-Yang (Ric) Huang ]
  Copyright    [ Copyleft(c) 2008-2014 LaDs(III), GIEE, NTU, Taiwan ]
****************************************************************************/

#ifndef CIR_MGR_H
#define CIR_MGR_H

#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include "sat.h"
#include "cirDef.h"
#include "cirGate.h"
#include "myHashMap.h"

using namespace std;

// TODO: Feel free to define your own classes, variables, or functions.

/********************************/
/*   Global variable and enum   */
/********************************/
extern CirMgr *cirMgr;

/*********************************/
/*   HashKey class declaration   */
/*********************************/
class SimKey;
class StrashKey;

/********************************/
/*   CirMgr class declaration   */
/********************************/

class CirMgr
{
public:
	friend class CirGate;
	friend class CirAIGGate;
	friend class CirPIGate;
	friend class CirPOGate;
	friend class CirUndefGate;
	friend class CirConstGate;
   CirMgr(): _simLog(0), _aigNum(0), _fecGroSize(0), _randSim(1) {}
   ~CirMgr() {}

   // Access functions
   // return '0' if "gid" corresponds to an undefined gate.
   CirGate* getGate(unsigned gid) const;

   // Member functions about circuit construction
   bool readCircuit(const string&);

   // Member functions about circuit optimization
   void sweep();
   void optimize();

   // Member functions about simulation
   void randomSim();
   void fileSim(ifstream&);
   void setSimLog(ofstream*);

   // Member functions about fraig
   void strash();
   void printFEC() const;
   void fraig();

   // Member functions about circuit reporting
   void printSummary() const;
   void printNetlist();
   void printPIs() const;
   void printPOs() const;
   void printFloatGates() const;
   void printFECPairs() const;
   void writeAag(ostream&);
   
   // Member functions about circuit searching
   void cirDFSearch();
   
   // Member functions about gate merging
   void gateMerge(CirGate*, CirGate*, bool);
   
   // Helper Functions
   bool mulTokens(const string&, vector<string>&) const;

private:
   ofstream       *_simLog;
	unsigned       _maxVar;
	unsigned			_inNum;
	unsigned			_outNum;
	unsigned			_latchNum;
	unsigned			_andGateNum;
	GateList 		_piList;
	GateList 		_poList;
	GateList 		_undefList;
	size_t 			_aigNum;
	GateList 		_totList;
	GateList 		_dfsList;
	FECGroups		_fecGroups;
	size_t			_fecGroSize;
	SatSolver		_solver;
	vector<size_t> _piValVec;
	vector<size_t> _poValVec;
	vector<string> _valPattern;
	bool           _randSim;
	// Private member functions about optimization
	void           sweepClearOut(CirGate*, size_t);
	void           optDelGate(CirGate*);
	bool           optGateMerge(CirGate*, size_t, bool);
	// Private member functions about simulation
	bool           fileCheck(ifstream&, vector<string>&);
   void 				cirRandSim();
   void 				cirFileSim();
   void           filePISim(vector<string>&, size_t, size_t);
   void           valPISim(vector<size_t>&, GateList&);
   bool 				fecCheck(GateList, bool);
   bool 				fecReCheck(bool, bool, bool);
   void 				fecGroupWrite(HashMap<SimKey, intIdList>*&);
   void 				fecGroNumSet();
   void 				fecGroNumReset();
   void 				fecGroupClean();
   void 				rmFromFECList(size_t, size_t);
   void           randPrint();
   void           filePrint();
   void           fraigPrint(bool);
   void           valRecordClear();
   void           cycleWrite(vector<string>&, vector<size_t>, size_t, size_t);
   void				randWriteValToStr();
   void				fileWriteValToStr();
   void				strTranpose(vector<string>&);
   // Private member functions about fraig
   void           genProofModel();
   void           solveFECPairs();
   void           solveDFSList(vector<size_t>&, size_t&, bool&);
   void           proveMessage(size_t, size_t, bool, bool);
};

#endif // CIR_MGR_H