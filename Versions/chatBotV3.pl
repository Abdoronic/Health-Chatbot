:- style_check(-singleton).

readInputTillQuit:-
    write('> Welcome to your personal assistant'), nl,
    loop([],[]).

loop(PQ, PR):-
    write('> '), flush_output,
    readln(In),
    once(removePunc(In, Q)),
    doExit(PQ, PR, Q),
    once(checkQuestion(Q)),
    once(answer(Q, PQ, PR, R)),
    once(loop([Q|PQ], [R|PR])).

checkQuestion(Q) :-
        question(Q).

checkQuestion(Q) :-
        \+question(Q),
        write('> I can not understand you'), nl.

doExit(PQ, PR, [quit]):-
    logout(PQ, PR),
    write('Bye'), nl, halt.

doExit(PQ, PR, Q):-
    Q \= [quit].

logout([], []).
logout([Q|T], [S|Xs]) :-
        (Q = [ I, ate, F, for, M];
        (Q = [ Can, I, have, F,for, M],
        S = ["You", can, have, F, for, M])),
        logout(T, Xs),
        writeListString(["You", "had", F, "for", M]), nl.

logout([Q|T], [S|Xs]) :-
        (Q \= [ I, ate, F, for, M],
        Q \= [ Can, I, have, F,for, M]),
        logout(T, Xs).

logout([Q|T], [S|Xs]) :-
        Q = [ Can, I, have, F,for, M],
        S \= ["You", can, have, F, for, M],
        logout(T, Xs).

writeListString([]).
writeListString([H|T]) :-
        write(H), put(' '),
        writeListString(T).

writeListString(X) :-  \+is_list(X), write(X).

isValid(Q) :-
    question(Q).

answer(Q, PQ, PR, R) :-
    question(Q),
    response(Q, PQ, PR, R),
    write('> '),
    writeListString(R), nl.

answer(Q, PQ, PR, R) :-
    \+question(Q).

isMember(X,[H]) :- H = X ; prop(X, contain, H).
isMember(X,[H|T]) :- H = X ; prop(X, contain, H).
isMember(X,[H|T]) :- H \= X, \+prop(X, contain, H), isMember(X, T).

removePunc([], []).
removePunc([?], []).
removePunc(['.'], []).
removePunc([H|T], [X|R]) :-
        downcase_atom(H, X),
        removePunc(T, R).

question([ How, many, calories, does, _, contain]).
question([ What, does, _, contain]).
question([ Can, I, have,_,for, _]).
question([ What, is, _]).
question([ How, many, calories, do, I, have, left]).
question([ What, kind, of, _, does, _, contain]).
question([ Is,_, a, _, in,_]).
question([ What, can, I, have, for, _, that, contains, _]).
question([ I, ate, _, for, _]).
question([ I, do, not ,eat , _]).

filterProp(Relation, Result) :-
    setof((X, Y), prop(X, Relation, Y), Result).

matchFirst(_,[],[]).
matchFirst(T1,[(Fir,Sec)|T],LM):-
            Fir==T1,
	    LM1= Sec-1,
            matchFirst(T1,T,LM2),
            LM=[LM1|LM2].
                     
matchFirst(T1,[(Fir,Sec)|T],LM):-
            Fir\=T1,
		    LM1= Sec-0,
            matchFirst(T1,T,LM2),
            LM=[LM1|LM2].

matchSecond(T1,[],[]).
matchSecond(T1,[(Fir,Sec)|T],LM):-
            Sec==T1,
		    LM1= Fir-1,
            matchSecond(T1,T,LM2),
            LM=[LM1|LM2].
                     
matchSecond(T1,[(Fir,Sec)|T],LM):-
            Sec\=T1,
		    LM1= Fir-0,
            matchSecond(T1,T,LM2),
            LM=[LM1|LM2].

mergeMatchLists([],[],[]).
mergeMatchLists(ML1, ML2, R):-
		    length(ML1, L1), 
	   	    length(ML2, L2),
		    L1 >= L2,
		    mergeMatchListsHelper(ML1, ML2, R).

mergeMatchLists(ML1, ML2, R):-
		    length(ML1, L1), 
	   	    length(ML2, L2),
		    L1 < L2,
		    mergeMatchListsHelper(ML2, ML1, R).

mergeMatchListsHelper([X-N|T], ML2, R):-
		    occurX(X, [X-N|T], Z1),
		    occurX(X, ML2, Z2),
		    Total is Z1 + Z2,
		    R1 = X-Total,
		    clearOccur(X, [X-N|T], Cleared1),
		    clearOccur(X, ML2, Cleared2),
		    mergeMatchLists(Cleared1, Cleared2, Rest),
		    R = [R1 | Rest].
                
occurX(X, [], 0).
occurX(X, [X-N| T], Total):-
		    occurX(X, T, Rest),
		    Total is Rest + N.
occurX(X, [H-N| T], Total):-
		    H \= X,
		    occurX(X, T, Total).

clearOccur(X, [], []).
clearOccur(X, [X-N|T], R):- clearOccur(X, T, R).
clearOccur(X, [H-N|T], [H-N|R]):- H \= X, clearOccur(X, T, R).

bestMatches(L1,BL):-
            bestMatcheshelp(L1,E,0),
            bestMatchesgetformax(L1,E,BL).

bestMatchesgetformax([],Maxsofar,[]).
bestMatchesgetformax([Fo-Num|T],Maxsofar,BL):-
            Maxsofar == Num,
            bestMatchesgetformax(T,Maxsofar,BL2),
            BL=[Fo|BL2].

bestMatchesgetformax([Fo-Num|T],Maxsofar,BL):-
            Maxsofar \= Num,
            bestMatchesgetformax(T,Maxsofar,BL2),
            BL=BL2.
                     
bestMatcheshelp([],Maxsofar ,Maxsofar).
bestMatcheshelp([X-Num|T], B,Maxsofar):-
            Num>Maxsofar,
            bestMatcheshelp(T, B,Num).

bestMatcheshelp([X-Num|T], B,Maxsofar):-
            Num =< Maxsofar,
            bestMatcheshelp(T, B,Maxsofar).

bestMatchesMin(ML,Maxsofar,BL):-
            bestMatchesgetformax(ML,Maxsofar,BL).

foodCal(F,C):-
    prop(F,contain,C,cal).

foodCal(F,C):-
    \+prop(F,contain,C,cal),
    setof(Y, prop(F,contain,Y),Result),
    foodCalHelper(Result, C).

foodCalHelper([],0).
foodCalHelper([H|T],C):-
    foodCalHelper(T,C1),
    prop(H,contain,Z,cal),
    C is Z+C1.
foodCalHelper([H|T],C):-
    foodCalHelper(T,C1),
    \+prop(H,contain,_,cal),
    foodCal(H, Z),
    C is Z+C1.

foodCalList([], 0).
foodCalList([H|T],C):-
    foodCalList(T,C1),
    foodCal(H, Z),
    C is Z + C1.

totalCal(1800).

calcCalories(F,PQ,PR,C):-
        totalCal(X),
        foodCal(F,Z),
        eatenFoodCalories(PQ, PR, T),
        C is X-T-Z.

getDiffAnswer(Q,PQ,PR,CR,R):-
	count(Q, PQ, C),
	length(CR, L),
        L > C,
        L > 1,
	getR(Q, PQ, PR, CR, R).

getDiffAnswer(Q,PQ,PR,CR,R):-
	count(Q, PQ, C),
	length(CR, L),
        L > C,
        L = 1,
	getRYN(Q, CR, C, R).

equals([],[]).
equals([H1|T1],[H2|T2]):-
        length([H1|T1], L1),
        length([H2|T2], L2),
        L1 = L2,
        downcase_atom(H1, X1),
        downcase_atom(H2, X2),
	X1 = X2,
	equals(T1,T2).
                      
count(_, [], 0).
count(Q, [H|T], C):-
	equals(Q, H),
	count(Q, T, C1),
	C is C1 + 1.

count(Q, [H|T], C):-
	\+equals(Q, H),
	count(Q, T, C).

getR(_, [], _, [H|T], H).
getR(Q, [PQ|PQs], [[PR]|PRs], CR, R):-
        equals(Q,PQ),
        clearQ(PR, CR, CRs),
	getR(Q, PQs, PRs, CRs, R).

getR(Q, [PQ|PQs], [[PR]|PRs], CR, R):-
        \+equals(Q,PQ),
	getR(Q, PQs, PRs, CR, R).

getRYN(_, [H|T], 0, H).
getRYN(Q, [H|T], C, R) :-
        equals(Q, H),
        C1 is C - 1,
        getRYN(Q, T, C1, R).

getRYN(Q, [H|T], C, R) :-
        \+equals(Q, H),
        getRYN(Q, T, C, R).


clearQ(_, [], []).
clearQ(X, [X|T], R) :- clearQ(X, T, R).
clearQ(X, [H|T], [H|R]):- H \= X, clearQ(X, T, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      START: Respones       %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

response(Q,_,_,["I",do,not,know]):-
        Q = [how,many,calories,does,F,contain],
        \+ prop(F,_,_).

response(Q,PQ,PR, R):-
        Q = [how, many, calories, does, F, contain],
        foodCal(F,C),
        getDiffAnswer(Q, PQ, _, [[C,"Calories"]], R).
                                                
response(Q,PQ,PR, ["I",told,you,that,before]):-
        Q = [how, many, calories, does, F, contain],
        foodCal(F,C),
        \+getDiffAnswer(Q, PQ, _, [[C,"Calories"]], _).

 %--------------------------------------------------------------

response(Q,_,_,["I",do,not,know]):-
        Q=[what, does, F, contain],
        \+prop(F,_,_).

response(Q,PQ,PR, R):-
        Q = [what, does, F, contain],
        filterProp(contain, L),
        matchFirst(F, L, M),
        bestMatchesMin(M, 1, CR),
        getDiffAnswer(Q, PQ, PR, CR, R).
                                                
response(Q,PQ,PR, ["I",told,you,that,before]):-
        Q = [what, does, F, contain],
        once(prop(F,_,_)),
        filterProp(contain, L),
        matchFirst(F, L, M),
        bestMatchesMin(M, 1, CR),
        \+getDiffAnswer(Q, PQ, PR, CR, _).

%------------------------------------------------------------
response(Q,_,_,["I",do,not,know]):-
        Q=[what, is , F],
        \+prop(F,is,_).

response(Q,PQ,PR,R):-
        Q=[what,  is , F],
        prop(F, is, FC),
        getDiffAnswer(Q, PQ, _, [[FC]], R).

response(Q,PQ,PR,["I",told,you,that,before]):-
        Q=[what,  is , F],
        prop(F, is, FC),
        \+getDiffAnswer(Q, PQ, _, [[FC]], R).       

%------------------------------------------------------------

response(Q, PQ, PR, [R, "Calories"]):-
        Q=[how, many, calories, do, i, have, left],
        totalCal(X),
        eatenFoodCalories(PQ, PR, C),
        R is X - C.

response(Q,PQ,PR,["I",do,not,know]):-
        Q=[how, many, calories, do, i, have, left],
        \+eatenFoodCalories(PQ, PR, C).

%------------------------------------------------------------
response(Q,_,_,["I",do,not,know]):-
        Q=[ can, i, have,F,for, M],
        (\+ prop(_,_,M);
        \+prop(F,_,_)).

response(Q,PQ,PR,["I",do,not,know]):-
        Q=[ can, i, have,F,for, M],
        once(prop(_,_,M)),
        once(prop(F,_,_)),
        \+prop(F, not, M),
        \+calcCalories(F, PQ, PR, C).

response(Q,PQ,PR,R):-
        Q=[ can, i, have,F,for, M],
        \+prop(F, not, M),
        calcCalories(F, PQ, PR, C),
        C >= 0,
        getDiffAnswer(Q, PQ, PR, [["You", can, have, F, for, M]], R).

response(Q,PQ,PR,R):-
        Q=[ can, i, have,F,for, M],
        prop(F, not, M),
        getDiffAnswer(Q, PQ, PR, [[F, is, not, suitable, for, M]], R).
    
response(Q,PQ,PR,R):-
        Q=[ can, i, have,F,for, M],
        \+prop(F, not, M),
        calcCalories(F, PQ, PR, C),
        C < 0,
        getDiffAnswer(Q, PQ, PR, [["No"]], R).

response(Q,PQ,PR,["I",told,you,that,before]):-
        Q=[ can, i, have,F,for, M],
        once(prop(_,_,M)),
        once(prop(F,_,_)),
        \+getDiffAnswer(Q, PQ, PR, [["You", can, have, F, for, M]], _).

%------------------------------------------------------------
response(Q,_,_,["I",do,not,know]) :-
        Q = [ what, kind, of, FC, does, F, contain],
        ((\+ prop(_,_,FC));
        (\+prop(F,_,_))).

response(Q, PQ, PR, R) :-
        Q = [ what, kind, of, FC, does, F, contain],
        setof(X, prop(X, is, FC), L1),
        setof(Y, prop(F, contain, Y), L2),
        mergeL(L1, L2, L),
        getDiffAnswer(Q, PQ, PR, L, R).

response(Q,_,_,["Nothing",from,what,i,know]) :-
        Q = [ what, kind, of, FC, does, F, contain],
        setof(X, prop(X, is, FC), L1),
        setof(Y, prop(F, contain, Y), L2),
        mergeL(L1, L2, L),
        length(L, 0).

response(Q, PQ, PR,["I",told,you,that,before]) :-
        Q = [ what, kind, of, FC, does, F, contain],
        setof(X, prop(X, is, FC), L1),
        setof(Y, prop(F, contain, Y), L2),
        mergeL(L1, L2, L),
        length(L, N),
        N > 0,
        \+getDiffAnswer(Q, PQ, PR, L, R).

%------------------------------------------------------------

response(Q,_,_,["I",do,not,know]) :-
        Q = [ is, F, a, FC, in, M],
        ((\+ prop(_,_,FC));
        (\+prop(F,_,_));
        (\+prop(M, _, _))).

response(Q, PQ, PR, R) :-
        Q = [ is, F, a, FC, in, M],
        prop(F, is, FC),
        prop(M, contain, F),
        getDiffAnswer(Q, PQ, PR, [["Yes"]], R).

response(Q, PQ, PR, R) :-
        Q = [ is, F, a, FC, in, M],
        once(prop(_,_,FC)),
        once(prop(F,_,_)),
        once(prop(M,_,_)),
        ((\+prop(F, is, FC));
        (\+prop(M, contain, F))),
        getDiffAnswer(Q, PQ, PR, [["No"]], R).

response(Q, PQ, PR, ["I","told",you,that,before]) :-
        Q = [ is, F, a, FC, in, M],
        once(prop(_,_,FC)),
        once(prop(F,_,_)),
        once(prop(M,_,_)),
        prop(F, is, FC),
        prop(M, contain, F),
        \+getDiffAnswer(Q, PQ, PR, [["I","told",you,that,before]], _).

response(Q, PQ, PR, ["I","told",you,that,before]) :-
        Q = [ is, F, a, FC, in, M],
        once(prop(_,_,FC)),
        once(prop(F,_,_)),
        once(prop(M,_,_)),
        ((\+prop(F, is, FC));
        (\+prop(M, contain, F))),
        \+getDiffAnswer(Q, PQ, PR, [["I","told",you,that,before]], _).

%------------------------------------------------------------

response(Q, _, _, ["I",do,not,know]) :-
        Q = [ what, can, i, have, for, M, that, contains, F],
        ((\+ prop(F, _,_));
        (\+prop(_, _,M))).

response(Q, PQ, PR, R) :-
        Q = [ what, can, i, have, for, M, that, contains, F],
        once(responseO(Q, PQ, PR, CR)),
        once(deEOCC(CR, CR2)),
        getDiffAnswer(Q, PQ, PR, CR2, R),!.
        
response(Q, PQ, PR, ["I",told,you,that,before]) :-
        Q = [ what, can, i, have, for, M, that, contains, F],
        once(responseO(Q, PQ, PR, CR)),
        once(deEOCC(CR, CR2)),
        length(CR2, L),
        L > 0,
        \+getDiffAnswer(Q, PQ, PR, CR2, R),!.

response(Q,_,_,["Nothing",from,what,i,know]) :-
        Q = [ what, can, i, have, for, M, that, contains, F],
        setof(X, prop(X, contain, F), Lx),
        filterProp(not, Y),
        matchSecond(M, Y, L1),
        mergeMatchLists(L1, L1, L2),
        listNotSuitable(L2, NS),
        \+member(F,NS),
        once(responseO(Q, PQ, PR, CR)),
        once(deEOCC(CR, CR2)),
        length(CR2, L),
        L = 0.

%------------------------------------------------------------

response(Q, _, _, ["Ok"]) :-
        Q = [i, ate, F, for, M].

%------------------------------------------------------------

response(Q, _, _, ["Ok"]) :-
        Q = [i, do, not, eat, F].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      END:   Respones       %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

responseO(Q, PQ, PR, CR) :-
        Q = [ what, can, i, have, for, M, that, contains, F],
        filterProp(contain, L1),
        filterProp(not, L2),
        matchSecond(F, L1, ContainF),
        matchSecond(M, L2, NotSuitable),
        mergeMatchLists(ContainF, ContainF, Temp),
        mergeMatchLists(Temp, Temp, CF),
        listNotSuitable(NotSuitable, NS),
        listUnliked(CF, PQ, PR, Un),
        currentCals(PQ, PR, C),
        listExceeding(CF, C, EX),
        mergeMatchLists(CF, NS, R1),
        mergeMatchLists(R1, Un, R2),
        mergeMatchLists(R2, EX, R3),
        removeZeroes(R3, R4),
        listOrderDesc(R4, CR).

deEOCC([],[]).
deEOCC([H-N|T],[H|R]) :-
        N >= 4,
        deEOCC(T, R).
deEOCC([H-N|T], R) :-
        N < 4,
        deEOCC(T, R).

removeZeroes([], []).
removeZeroes([_- N|T], R) :-  N =< 0, removeZeroes(T, R).
removeZeroes([H-N|T], [H-N|R]) :- N > 0, removeZeroes(T, R).

currentCals(PQ, PR, C) :-
        totalCal(X),
        eatenFoodCalories(PQ, PR, T),
        C is X-T.

listNotSuitable([], []).
listNotSuitable([H-N|T], [H-X|R]) :-
        N = 1,
        X = -1,
        listNotSuitable(T, R).

listNotSuitable([H-N|T], R) :-
        N \= 1,
        listNotSuitable(T, R).

listExceeding([], _, []).
listExceeding([F-_|T], Rem, [P|R]) :-
        foodCal(F, Z),
        K is Rem - Z, 
        K < 0,
        N is -1,
        P = F-N,
        listExceeding(T, Rem, R).

listExceeding([F-_|T], Rem, R) :-
        foodCal(F, Z),
        K is Rem - Z, 
        K >= 0,                
        listExceeding(T, Rem, R).

listUnliked([], _, _, []).
listUnliked([F-_|T], PQ, PR, [F-N|R]) :-
        getUnlikedIngredients(PQ, Un),
        oneOf(F,Un),
        N is -1,
        listUnliked(T, PQ, PR, R).

listUnliked([F-_|T], PQ, PR, R) :-
        getUnlikedIngredients(PQ, Un),
        \+oneOf(F,Un),
        listUnliked(T, PQ, PR, R).

oneOf(F, [H|T]) :-
        prop(F, contain, H).

oneOf(F, [H|T]) :-
        \+prop(F, contain, H),
        oneOf(F, T).

listOrderDesc([],[]).
listOrderDesc([D-E],[D-E]).
listOrderDesc(LP,OLP):-
            getmaxfull(LP, B,0-(-1)),                    
            clearOccurfull(B,LP,Cleared),
            once(listOrderDesc(Cleared,S)),
            OLP=[B|S].
                      
getmaxfull([],S-Maxsofar ,S-(Maxsofar)).
getmaxfull([X-Num|T], B,S-(Maxsofar)):-
            Num>Maxsofar,
            getmaxfull(T, B,X-(Num)).

getmaxfull([X-Num|T], B,S-(Maxsofar)):-
            Num =< Maxsofar,
            getmaxfull(T, B,S-(Maxsofar)).

clearOccurfull(X-N, [], []).
clearOccurfull(X-Ne, [X-Ne|T], T).
clearOccurfull(X-Ne, [H-N|T], [H-N|R]):- H-N \= X-Ne, clearOccurfull(X-Ne, T, R).

eatenFoodCalories([],_, 0).
eatenFoodCalories([Q|T], [S|Rs], R) :-
        ((Q = [ I, ate, F, for, M],
        S \= ["I",do,not,know]);
        (Q = [ Can, I, have, F,for, M]
        ,S = ["You", can, have, F, for, M] )),
        eatenFoodCalories(T, Rs, R1),
        foodCal(F, C),
        R is R1 + C.
eatenFoodCalories([Q|T], [S|Rs], R) :-
        (Q \= [ I, ate, F, for, M],
        Q \= [ Can, I, have, F,for, M]),
        eatenFoodCalories(T, Rs, R).

eatenFoodCalories([Q|T], [S|Rs], R) :-
        (Q = [ Can, I, have, F,for, M],
        (S = ["No"]; S = [F, is, not, suitable, for, M])),
        eatenFoodCalories(T, Rs, R).

foodFromHistory([], []).
foodFromHistory([Q|T], [F|R]) :-
        (Q = [ I, ate, F, for, M];
        Q = [ Can, I, have, F, for, M]),
        foodFromHistory(T, R).

foodFromHistory([Q|T], R) :-
        (Q \= [ I, ate, F, for, M],
        Q \= [ Can, I, have, F,for, M]),
        foodFromHistory(T, R).

getUnlikedIngredients([], []).
getUnlikedIngredients([Q|T], [F|R]) :-
        Q = [I, do, not, eat, F],
        getUnlikedIngredients(T, R).

getUnlikedIngredients([Q|T], R) :-
        Q \= [I, do, not, eat, F],
        getUnlikedIngredients(T, R).

mergeL([], _, []).
mergeL([X|Xs], L, [X|R]) :-
        member(X, L),
        mergeL(Xs, L, R).

mergeL([X|Xs], L, R) :-
        \+member(X, L),
        mergeL(Xs, L, R).
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-discontiguous(prop/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% VEGETABLES %%%%

prop(tomato,is,vegetable).
prop(onion,is,vegetable).
prop(bell_pepper,is,vegetable).
prop(chili_pepper,is,vegetable).
prop(carrot,is,vegetable).
prop(pea,is,vegetable).
prop(artichoke,is,vegetable).
prop(eggplant,is,vegetable).
prop(cucumber,is,vegetable).
prop(lettuce,is,vegetable).
prop(okra,is,vegetable).
prop(cauliflower,is,vegetable).
prop(cabbage,is,vegetable).
prop(broccoli,is,vegetable).
prop(mushroom,is,vegetable).
prop(potato,is,vegetable).
prop(zucchini,is,vegetable).
prop(broccoli,is,vegetable).
prop(spinach,is,vegetable).
prop(corn,is,vegetable).

%%%% FRUITS %%%%

prop(strawberry,is,fruit).
prop(blackberry,is,fruit).
prop(blueberry,is,fruit).
prop(banana,is,fruit).
prop(orange,is,fruit).
prop(grape,is,fruit).
prop(pineapple,is,fruit).
prop(apple,is,fruit).
prop(kiwi,is,fruit).
prop(peaches,is,fruit).
prop(guava,is,fruit).
prop(pear,is,fruit).
prop(mango,is,fruit).
prop(apricot,is,fruit).
prop(avocado,is,fruit).
prop(cherry,is,fruit).
prop(fig,is,fruit).
prop(coconut,is,fruit).
prop(lemon,is,fruit).
prop(watermelon,is,fruit).
prop(cantaloupe,is,fruit).

%%%% DIARY %%%%

prop(cheese,is,diary).
prop(milk,is,diary).
prop(yogurt,is,diary).

%%%% CARBS %%%%

prop(flour,is,carb).
prop(rice,is,carb).
prop(pasta,is,carb).
prop(chocolate,is,carb).

%%%% FATS %%%%

prop(oil,is,fat).
prop(butter,is,fat).

%%%% PROTEINS %%%%

prop(egg,is,protein).
prop(fish,is,protein).
prop(chicken,is,protein).
prop(meat,is,protein).
prop(shrimp,is,protein).
prop(minced_meat,is,protein).

%%%% DRESSING %%%%

prop(mayonnaise,is,dressing).
prop(vinegar,is,dressing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(chicken_caesar_salad,contain,chicken).
prop(chicken_caesar_salad,contain,oil).
prop(chicken_caesar_salad,contain,lettuce).
prop(chicken_caesar_salad,contain,cheese).
prop(chicken_caesar_salad,contain,mayonnaise).
prop(chicken_caesar_salad,contain,vinegar).
prop(chicken_caesar_salad,contain,bread).

prop(green_salad,contain,carrot).
prop(green_salad,contain,bell_pepper).
prop(green_salad,contain,lettuce).
prop(green_salad,contain,onion).
prop(green_salad,contain,tomato).
prop(green_salad,contain,cucumber).

prop(coleslaw_salad,contain,carrot).
prop(coleslaw_salad,contain,cabbage).
prop(coleslaw_salad,contain,mayonnaise).
prop(coleslaw_salad,contain,oil).

prop(pasta_salad,contain,bell_pepper).
prop(pasta_salad,contain,mayonnaise).
prop(pasta_salad,contain,pasta).
prop(pasta_salad,contain,corn).

prop(fruit_salad,contain,strawberry).
prop(fruit_salad,contain,banana).
prop(fruit_salad,contain,orange).
prop(fruit_salad,contain,apple).

prop(croissant,contain,butter).
prop(croissant,contain,flour).
prop(croissant,contain,milk).
prop(croissant,contain,oil).
prop(croissant,contain,egg).

prop(spanish_omelette,contain,egg).
prop(spanish_omelette,contain,oil).
prop(spanish_omelette,contain,potato).

prop(boiled_egg,contain,egg).

prop(grilled_chicken,contain,chicken).
prop(grilled_chicken,contain,lemon).
prop(grilled_chicken,contain,onion).

prop(fried_chicken,contain,chicken).
prop(fried_chicken,contain,oil).
prop(fried_chicken,contain,onion).
prop(fried_chicken,contain,flour).

prop(cake,contain,flour).
prop(cake,contain,butter).
prop(cake,contain,milk).
prop(cake,contain,egg).

prop(chocolate_cake,contain,cake).
prop(chocolate_cake,contain,chocolate).

prop(white_rice,contain,rice).
prop(white_rice,contain,butter).

prop(mexican_rice,contain,rice).
prop(mexican_rice,contain,oil).
prop(mexican_rice,contain,onion).
prop(mexican_rice,contain,tomato).

prop(ratatouille,contain,zucchini).
prop(ratatouille,contain,eggplant).
prop(ratatouille,contain,tomato).
prop(ratatouille,contain,bell_pepper).
prop(ratatouille,contain,onion).
prop(ratatouille,contain,lemon).
prop(ratatouille,contain,oil).
prop(ratatouille,contain,vinegar).

prop(lasagne,contain,pasta).
prop(lasagne,contain,milk).
prop(lasagne,contain,flour).
prop(lasagne,contain,butter).
prop(lasagne,contain,minced_meat).
prop(lasagne,contain,cheese).

prop(pasta_white_sauce,contain,pasta).
prop(pasta_white_sauce,contain,milk).
prop(pasta_white_sauce,contain,flour).
prop(pasta_white_sauce,contain,butter).

prop(pasta_red_sauce,contain,pasta).
prop(pasta_red_sauce,contain,tomato).
prop(pasta_red_sauce,contain,oil).

prop(pasta_alfredo,contain,pasta).
prop(pasta_alfredo,contain,milk).
prop(pasta_alfredo,contain,flour).
prop(pasta_alfredo,contain,butter).
prop(pasta_alfredo,contain,chicken).

prop(pasta_negresco,contain,pasta).
prop(pasta_negresco,contain,milk).
prop(pasta_negresco,contain,flour).
prop(pasta_negresco,contain,butter).
prop(pasta_negresco,contain,chicken).
prop(pasta_negresco,contain,cheese).

prop(shrimp_pasta,contain,pasta).
prop(shrimp_pasta,contain,shrimp).
prop(shrimp_pasta,contain,butter).
prop(shrimp_pasta,contain,milk).

prop(pizza,contain,tomato).
prop(pizza,contain,cheese).
prop(pizza,contain,flour).
prop(pizza,contain,oil).

prop(bread,contain,milk).
prop(bread,contain,flour).
prop(bread,contain,butter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(tomato,contain,11,cal).
prop(onion,contain,44,cal).
prop(cheese,contain,431,cal).
prop(egg,contain,78,cal).
prop(pasta,contain,131,cal).
prop(bell_pepper,contain,24,cal).
prop(chili_pepper,contain,18,cal).
prop(carrot,contain,25,cal).
prop(pea,contain,81,cal).
prop(artichoke,contain,120,cal).
prop(eggplant,contain,25,cal).
prop(cucumber,contain,32,cal).
prop(lettuce,contain,15,cal).
prop(okra,contain,33,cal).
prop(cauliflower,contain,25,cal).
prop(cabbage,contain,25,cal).
prop(broccoli,contain,31,cal).
prop(mushroom,contain,5,cal).
prop(potato,contain,163,cal).
prop(zucchini,contain,33,cal).
prop(spinach,contain,23,cal).
prop(corn,contain,86,cal).
prop(strawberry,contain,33,cal).
prop(blackberry,contain,43,cal).
prop(blueberry,contain,57,cal).
prop(banana,contain,89,cal).
prop(orange,contain,47,cal).
prop(grape,contain,62,cal).
prop(pineapple,contain,42,cal).
prop(apple,contain,92,cal).
prop(kiwi,contain,42,cal).
prop(peaches,contain,59,cal).
prop(guava,contain,38,cal).
prop(pear,contain,85,cal).
prop(mango,contain,99,cal).
prop(apricot,contain,48,cal).
prop(avocado,contain,160,cal).
prop(cherry,contain,50,cal).
prop(fig,contain,107,cal).
prop(coconut,contain,283,cal).
prop(lemon,contain,24,cal).
prop(watermelon,contain,30,cal).
prop(cantaloupe,contain,34,cal).
prop(milk,contain,124,cal).
prop(yogurt,contain,218,cal).
prop(flour,contain,364,cal).
prop(rice,contain,150,cal).
prop(oil,contain,240,cal).
prop(butter,contain,204,cal).
prop(fish,contain,305,cal).
prop(chicken,contain,335,cal).
prop(meat,contain,250,cal).
prop(shrimp,contain,85,cal).
prop(minced_meat,contain,332,cal).
prop(mayonnaise,contain,188,cal).
prop(vinegar,contain,3,cal).
prop(chocolate,contain,137,cal).
%prop(,contain,,cal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(cheese,not,lunch).
prop(yogurt,not,lunch).
prop(boiled_egg,not,lunch).
prop(boiled_egg,not,dinner).
prop(spanish_omelette,not,lunch).
prop(spanish_omelette,not,dinner).
prop(croissant,not,lunch).
prop(chicken_caesar_salad,not,breakfast).
prop(chicken_caesar_salad,not,dinner).
prop(pizza,not,breakfast).
prop(shrimp_pasta,not,breakfast).
prop(shrimp_pasta,not,dinner).
prop(pasta_negresco,not,breakfast).
prop(pasta_negresco,not,dinner).
prop(pasta_alfredo,not,breakfast).
prop(pasta_alfredo,not,dinner).
prop(pasta_red_sauce,not,breakfast).
prop(pasta_red_sauce,not,dinner).
prop(pasta_white_sauce,not,breakfast).
prop(pasta_white_sauce,not,dinner).
prop(fried_chicken,not,breakfast).
prop(fried_chicken,not,dinner).
prop(grilled_chicken,not,breakfast).
prop(grilled_chicken,not,dinner).
prop(lasagne,not,breakfast).
prop(lasagne,not,dinner).
prop(ratatouille,not,breakfast).
prop(ratatouille,not,dinner).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   


