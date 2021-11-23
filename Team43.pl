:- use_module(library(lists)).
offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 2), bus). 
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1), bus). 
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 2), hotel). 
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1), cabin). 
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100). 
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100). 
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20). 
customerPreferredActivity(customer(amera, aly, 1993-01-30, single, 1, student), diving, 50). 
customerPreferredActivity(customer(amera, aly, 1993-01-30, single, 1, student), snorkeling, 0). 
customerPreferredActivity(customer(amera, aly, 1993-01-30, single, 1, student), horseRiding, 100). 
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60). 
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20). 
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50). 
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100). 
customerPreferredMean(customer(amera, aly, 1993-01-30, single, 1, student), bus, 50). 
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10). 
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50). 
customerPreferredAccommodation(customer(amera, aly, 1993-01-30, single, 1, student), hotel, 100).
customerPreferredAccommodation(customer(amera, aly, 1993-01-30, single, 1, student), cabin, 70). 
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100). 
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin, 79).
possibleSubset(L,R):-
   powerset(L,S),
   permutation1(S,R).
powerset([],[]).
powerset([H|T],[H|T1]):-
     powerset(T,T1).
powerset([H|T],T1):-
    powerset(T,T1).
permutation1([H|T],L) :- 
     permutation1(T,P), 
	 insert(H,P,L). 
permutation1([],[]).
insert(X,[H|T],[H|T1]) :- 
     insert(X,T,T1).
insert(X,L,[X|L]). 
preferenceSatisfaction(offer(Destination,Activities,Cost,ValidFrom,ValidTo,Period,Duration,NoOfGuests),Customer,ChosenPrefs,S):-
     offerMean(offer(Destination,Activities,Cost,ValidFrom,ValidTo,Period,Duration,NoOfGuests),_),
     O = offer(Destination,Activities,Cost,ValidFrom,ValidTo,Period,Duration,NoOfGuests) ,
	 offerMean(O,M),
     offerAccommodation(O,A),
	 accommodation(ChosenPrefs,A1),
	 activity(ChosenPrefs,Ac),
	 means(ChosenPrefs,M1),
	 pointsA(A,A1,Customer,X1),
	 pointsAc(Activities,Ac,Customer,X2),
	 pointsM(M,M1,Customer,X3),
	 S is X1 + X2 + X3 .	 
pointsA(A,A1,Customer,X):-
     A1\=nil,
	 A=A1,
	 customerPreferredAccommodation(Customer,A,X).
pointsA(A,nil,Customer,0).
pointsAc(A,[H|T],Customer,X):-
     member(H,A),
	 customerPreferredActivity(Customer,H,X2),
	 pointsAc(A,T,Customer,X1),
	 X = X1 + X2 .
pointsAc(A,nil,Customer,0).
pointsAc(A,[],Customer,0).
pointsM(A,A1,Customer,X):-
     A1\=nil,
	 A=A1,
	 customerPreferredMean(Customer,A,X).
pointsM(A,nil,Customer,0).
accommodation([],nil).
accommodation([accommodation(X)|T],X).
accommodation([X|T],Y):-
     X\=accommodation(_),
	 accommodation(T,Y).
activity([],nil).
activity([activity(X)|T],X).
activity([X|T],Y):-
     X\=activity(_),
	 activity(T,Y).
means([],nil).
means([means(X)|T],X).
means([X|T],Y):-
     X\=means(_),
	 means(T,Y).
overlapPeriod(period(Y1-M1-D1,Y2-M2-D2),period(Y3-M3-D3,Y4-M4-D4)):-
     (helper(Y1-M1-D1,period(Y3-M3-D3,Y4-M4-D4)); helper(Y2-M2-D2,period(Y3-M3-D3,Y4-M4-D4)) ; helper(Y3-M3-D3 , period(Y1-M1-D1,Y2-M2-D2) ); helper(Y4-M4-D4 , period(Y1-M1-D1,Y2-M2-D2))).
helper(Y1-M1-D1,period(Y3-M3-D3,Y4-M4-D4)):-
     Y1>Y3 ,Y3\=Y4,
	 Y1<Y4.
helper(Y1-M1-D1,period(Y3-M3-D3,Y4-M4-D4)):-
     Y1=Y3 ,Y3\=Y4, M1>M3 ,
	 Y1<Y4.	 
helper(Y1-M1-D1,period(Y3-M3-D3,Y4-M4-D4)):-
     Y1=Y3 ,Y3\=Y4, M1=M3 , D1>=D3 ,
	 Y1<Y4.	 	 
helper(Y1-M1-D1,period(Y3-M3-D3,Y4-M4-D4)):-
     Y1>Y3 ,Y3\=Y4,
	 Y1=Y4 , M1<M4 .
helper(Y1-M1-D1,period(Y3-M3-D3,Y4-M4-D4)):-
     Y1>Y3 ,Y3\=Y4,
	 Y1=Y4 , M1=M4 , D1=<D4 .
helper(Y1-M1-D1,period(Y3-M3-D3,Y4-M4-D4)):-
     Y3=Y4,
	 Y1=Y3,
	 M1>=M3,
	 M3\=M4,
	 M1<M4 .
helper(Y1-M1-D1,period(Y3-M3-D3,Y4-M4-D4)):-
     Y3=Y4,
	 Y1=Y3,
	 M1>M3,
	 M3\=M4,
	 M1=<M4 .
helper(Y1-M1-D1,period(Y3-M3-D3,Y3-M3-D4)):-
     Y1 = Y3 ,
	 M1 = M3 ,
	 D1>=D3,
	 D1=<D4 .
activityexist([]):- false.
activityexist([activity(A)|T]).
activityexist([X|T]):-
     X\=activity(_),
	 activityexist(T).
choosePreferences(Prefs,ChosenPreferences):-
     possibleSubset(Prefs,ChosenPreferences1),
	 activityexist(ChosenPreferences1),
	 getsubset(ChosenPreferences1,ChosenPreferences).
choosePreferences(Prefs,ChosenPreferences):-
     possibleSubset(Prefs,ChosenPreferences),
	 \+(activityexist(ChosenPreferences)).
getsubset([],[]).
getsubset([activity(X)|T],[activity(R)|T]):-
     powerset(X,R),
	 R\=[],R\=X.
getsubset([X|T],[X|T1]):-
     getsubset(T,T1).
getOffer(ChosenPrefs,Offer):-
     offerMean(offer(Destination,Activities,Cost,ValidFrom,ValidTo,Period,K,L),_),
	 destination(ChosenPrefs,Destination,X1),X1 \= nan,
	 cost(ChosenPrefs,Cost,X2),X2 \= nan,
	 period1(ChosenPrefs,Period,X3),X3 \= nan,
	 activityexist2(ChosenPrefs,Activities,X4),X4 \= nan,
	 Offer = offer(Destination,Activities,Cost,ValidFrom,ValidTo,Period,K,L),
	 offerMean(Offer,Mean),
	 means2(ChosenPrefs,Mean,X5),X5 \= nan,
	 offerAccommodation(Offer,Accommodation),
	 accommodation2(ChosenPrefs,Accommodation,X6),X6 \= nan,
	 Offer = offer(Destination,Activities,Cost,ValidFrom,ValidTo,Period,K,L).	 	 
destination([],X,nil).
destination([dest(X)|T],X,yes).
destination([dest(X)|T],Y,nan):- X\=Y.
destination([X|T],Y,L):-
     X\=dest(_),
	 destination(T,Y,L).  
cost([],X,nil).
cost([budget(X)|T],Y,yes):- X>=Y.
cost([budget(X)|T],Y,nan):- X<Y.
cost([X|T],Y,L):-
     X\=budget(_),
	 cost(T,Y,L).	
period1([],X,nil).
period1([period(X,Y)|T],P,yes):-
     overlapPeriod(period(X,Y),P).
period1([period(X,Y)|T],P,nan):-
     \+(overlapPeriod(period(X,Y),P)).
period1([X|T],Y,L):-
     X\=period(_),
	 period1(T,Y,L).
accommodation2(ChosenPrefs,L,nil):-
     accommodation(ChosenPrefs,Q),
     Q=nil.
accommodation2(ChosenPrefs,L,yes):-
     accommodation(ChosenPrefs,Q),
     Q\=nil,
     Q=L.
accommodation2(ChosenPrefs,L,nan):-
     accommodation(ChosenPrefs,Q),
     Q\=nil,
     Q\=L.	 	 
means2(ChosenPrefs,L,nil):-
     means(ChosenPrefs,Q),
     Q=nil.
means2(ChosenPrefs,L,yes):-
     means(ChosenPrefs,Q),
     Q\=nil,
     Q=L.
means2(ChosenPrefs,L,nan):-
     means(ChosenPrefs,Q),
     Q\=nil,
     Q\=L.	 	 
activityexist2(ChosenPrefs,L,nil):-
     activity(ChosenPrefs,Q),
	 Q=nil.
activityexist2(ChosenPrefs,L,yes):-
     activity(ChosenPrefs,Q),
	 Q\=nil,
     activityexist3(Q,L).	
activityexist2(ChosenPrefs,L,nan):-
     activity(ChosenPrefs,Q),
	 Q\=nil,
     \+(activityexist3(Q,L)).		 
activityexist3([H|T],L):-
     member(H,L).
activityexist3([H|T],L):-
    \+(member(H,L)),
     activityexist3(T,L).
recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
    choosePreferences(Prefs,ChosenPrefs),
	getOffer(ChosenPrefs,O).  
recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):-
    offerMean(offer(Destination,Activities,Cost,ValidFrom,ValidTo,Period,Duration,Max),_),
	Offer= offer(Destination,Activities,Cost,ValidFrom,ValidTo,Period,Duration,Max),
	chooseCustomers(Offer,Customers,PreferenceList,CustomersChosen1,[]),
	chooseCustomers2(CustomersChosen1,Customers,Max,[],CustomersChosen).	
chooseCustomers(Offer,[C|Cs],[P|Ps],Cus,L):-
    checkValidity(Offer,C,P,S),
	append(L,[S],L1),
	chooseCustomers(Offer,Cs,Ps,Cus,L1).
chooseCustomers(Offer,[],[],Cus,Cus).
checkValidity(Offer,C,P,S):-
    recommendOfferForCustomer(P,P1,Offer),
	preferenceSatisfaction(Offer,C,P1,S).
chooseCustomers2(L,C,Max,Q1,Q):-
    Max >0 ,
    max_list(L,V),
	nth0(N,L,V),
	nth0(N,C,A),
	append(Q1,[A],Q2),
	remove(L,V,L1),
	remove(C,A,C1),
	Max1 is Max - 1 ,
	chooseCustomers2(L1,C1,Max1,Q2,Q).
chooseCustomers2([],[],Max,Q,Q):-
    Max>=0 .
chooseCustomers2(_,_,0,Q,Q).
remove([],H,[]).
remove([H|T],H,T).
remove([H|T],X,[H|T1]):-
     H\=X,
	 remove(T,X,T1).	


	 
	 

