%General

convertBinToDec(B,D):-
	helper(B,0,0,D).	
helper(0,_,X,X).
helper(B,K,S,X):-
	B\=0,
	B1 is B mod 10,
	S1 is S + ((2**K)*B1),
	B2 is B//10,
	K1 is K+1,
	helper(B2,K1,S1,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replaceIthItem(A,[_|T],0,[A|T]).	
replaceIthItem(A,[H|T],I,[H|R1]):-
	I\=0,
	I1 is I-1,
	replaceIthItem(A,T,I1,R1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
splitEvery(_,[],[]).
splitEvery(N,L,[X|R1]):-
	split1(L,N,X,L1),
	splitEvery(N,L1,R1).	
split1(L,0,[],L).
split1([X|Xs],N,[X|Ys],Zs):-
	N>0,
	N1 is N-1,
	split1(Xs,N1,Ys,Zs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logBase2(A,N):-
	helpLog(A,0,N).
                                               
helpLog(A,C,C):-
    A =:= 2**C.
	
helpLog(A,C,N):-
	A =\= 2**C,
	C1 is C+1,
	helpLog(A,C1,N).	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
fillZeros(S,0,S).
fillZeros(S,N,R):-
	N\=0,
	string_concat("0", S, S1),
	N1 is N-1,
	fillZeros(S1,N1,R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getNumBits(_,fullyAssoc,_,0).
getNumBits(A,setAssoc,_,BitsNum):-
	logBase2(A,BitsNum).
getNumBits(_,directMap,L,BitsNum):-
	length(L,N),
	logBase2(N,BitsNum).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getDataFromCache(StringAddress, Cache, Data, 0, directMap, BitsNum):-
      atom_number(StringAddress, Bin),
      convertAddress(Bin, BitsNum, Tag, Idx, directMap),
      atom_number(StringTag, Tag),
      string_length(StringTag, TagLength),
	  string_length(StringAddress, Stringlength),
	  N is (Stringlength - TagLength - BitsNum),
	  fillZeros(StringTag, N, WithZeros),
	  convertBinToDec(Idx, Dec),
	  nth0(Dec, Cache, item(tag(X), data(D), 1, _)),
	  WithZeros = X,
	  Data = D.

	
getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
	atom_number(StringAddress,Integer),
	convertAddress(Integer,SetsNum,Tag,Idx,setAssoc),
	length(Cache,L),
	Y is L//SetsNum,
	splitEvery(Y,Cache,Cache1),
	convertBinToDec(Idx,A),
	getDataFromCacheHelper1(Cache1,A,Z),
	getDataFromCacheHelper2(Z,Tag,Data,0,HopsNum).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
	Idx is Bin mod 10**BitsNum,
	Tag is Bin // 10**BitsNum.
	
convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-	
	logBase2(SetsNum,A),
	Idx is Bin mod 10**A,
	Tag is Bin // 10**A.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%REPLACE

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,directMap,BitsNum):-
	convertBinToDec(Idx,A),
	atom_number(StringTag,Tag),
	atom_number(StringIdx,Idx),
	string_length(StringIdx,L10),
	ZZZ is BitsNum-L10,
	fillZeros(StringIdx,ZZZ,KKKK),
	string_concat(Tag,KKKK,Bin1),
	atom_number(Bin1,Bin),
	convertBinToDec(Bin,Z),
	replaceInCacheHelper1(Mem,Z,Y),
	string_length(StringTag,L1),
	L2 is 6-(BitsNum+L1),
	fillZeros(StringTag,L2,StringTag1),
	replaceIthItem(item(tag(StringTag1),data(Y),1,0),OldCache,A,NewCache),
	Data=Y.
	

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,setAssoc,SetsNum):-
    convertBinToDec(Idx,A),
	logBase2(SetsNum,L2),
	atom_number(StringTag,Tag),
	atom_number(StringIdx,Idx),
	string_length(StringIdx,L10),
	ZZZ is L2-L10,
	fillZeros(StringIdx,ZZZ,KKKK),
	string_concat(Tag,KKKK,Bin1),
	atom_number(Bin1,Bin),
	convertBinToDec(Bin,Decimal),
	replaceInCacheHelper1(Mem,Decimal,Data),
	length(OldCache,Len),
	Splitnum is Len//SetsNum,
	splitEvery(Splitnum,OldCache,OldCache1),
	string_length(StringTag,L1),
	L3 is 6-(L2+L1),
	fillZeros(StringTag,L3,StringTag1),
	getset(OldCache1,A,Result),
	increment(Result,Result1),
	getplace(Result1,0,0,R),
	replaceIthItem(item(tag(StringTag1),data(Data),1,0),Result1,R,Result5),
	replaceIthItem(Result5,OldCache1,A,NewCache1),
	splitEvery(SetsNum,NewCache,NewCache1).
	

replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_):-
	atom_number(StringTag,Tag),
	convertBinToDec(Tag,Decimal),
	replaceInCacheHelper1(Mem,Decimal,Data),
	string_length(StringTag,L1),
	L3 is 6-L1,
	fillZeros(StringTag,L3,StringTag1),
	increment(OldCache,Result),
	getplace(Result,0,0,R),
	replaceIthItem(item(tag(StringTag1),data(Data),1,0),Result,R,NewCache),
	ItemData=Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%METHODS

getDataFromCacheHelper1([H|_],0,H).
getDataFromCacheHelper1([_|T],A,Z):-
	A \= 0,
	A1 is A-1,
	getDataFromCacheHelper1(T,A1,Z).
	

replaceInCacheHelper1([H|_],0,H).	
replaceInCacheHelper1([_|T],Z,Y):-
	Z\=0,
	Z1 is Z-1,
	replaceInCacheHelper1(T,Z1,Y).	
	

getDataFromCacheHelper2([item(tag(X),data(Data),1,_)|_],Tag,Data,K,K):-
	atom_number(X,Tag).

getDataFromCacheHelper2([item(tag(_),data(_),0,_)|T],Tag,Data,K,HopsNum):-
	K1 is K+1,
	getDataFromCacheHelper2(T,Tag,Data,K1,HopsNum).

getDataFromCacheHelper2([item(tag(X),data(_),1,_)|T],Tag,Data,K,HopsNum):-
	Tag\=X,
	K1 is K+1,
	getDataFromCacheHelper2(T,Tag,Data,K1,HopsNum).	
	

getset([H|_],0,H).	
getset([_|T],A,Result):-
	A \=0,
	A1 is A-1,
	getset(T,A1,Result).

increment([],[]).	
increment([item(tag(Z),data(A),1,X)|T],[item(tag(Z),data(A),1,X1)|R1]):-
	X1 is X+1 ,
	increment(T,R1).
	
increment([item(tag(Z),data(A),0,X)|T],[item(tag(Z),data(A),0,X)|R1]):-
	increment(T,R1).
	
getplace([item(tag(_),data(_),0,_)|_],_,A,A).
getplace([_],_,A,A).

getplace([item(tag(_),data(_),1,X1),item(tag(_),data(_),1,X)|T],Count,A,R):-
	X1 >= X,
	Count1 is Count+1,
	getplace([item(tag(_),data(_),1,X1)|T],Count1,A,R).
	
getplace([item(tag(_),data(_),1,X1),item(tag(_),data(_),1,X)|T],Count,_,R):-
	X > X1,
	Count1 is Count+1,
	getplace([item(tag(_),data(_),1,X)|T],Count1,Count1,R).
	
getplace([item(tag(_),data(_),1,_),item(tag(_),data(_),0,_)|_],Count,_,Count1):-
	Count1 is Count +1 .	
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getData(StringAddress,OldCache,_,NewCache,Data,HopsNum,Type,BitsNum,hit):-
getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
NewCache = OldCache.
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
atom_number(StringAddress,Address),
convertAddress(Address,BitsNum,Tag,Idx,Type),
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],_,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,
[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
getNumBits(NumOfSets,Type,OldCache,BitsNum),
(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
getData(Address,OldCache,Mem,NewCache,Data,_,Type,Num,Status),
runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,
Type,NumOfSets).
