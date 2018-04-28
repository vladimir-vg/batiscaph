
% proc('<0.461.0>', '<0.337.0>', 1523890084218636, 1523890092343867).
% proc('<0.360.0>', '<0.356.0>', 1523879339252352, 1523879339252616).
% proc('<0.470.0>', '<0.337.0>', 1523890997994720, 1523891023186974).
% proc('<0.355.0>', '<0.338.0>', 1523879339251714, 1523879339252255).
% proc('<0.459.0>', '<0.337.0>', 1523890051919735, 1523890084218403).
% proc('<0.346.0>', '<0.338.0>', 1523879339250744, 1523879339251432).
% proc('<0.348.0>', '<0.338.0>', 1523879339250975, 1523879339251588).
% proc('<0.358.0>', '<0.338.0>', 1523879339252085, 1523879339252567).
% proc('<0.343.0>', '<0.338.0>', 1523879339250249, 1523879339250474).
% proc('<0.421.0>', '<0.337.0>', 1523886011086640, 1523887228942003).
% proc('<0.350.0>', '<0.345.0>', 1523879339251323, 1523879339251699).
% proc('<0.354.0>', '<0.349.0>', 1523879339251614, 1523879339251726).
% proc('<0.339.0>', '<0.337.0>', 1523879028578823, 1523879032969175).
% proc('<0.362.0>', '<0.358.0>', 1523879339252555, 1523879339252639).
% proc('<0.359.0>', '<0.355.0>', 1523879339252240, 1523879339252602).
% proc('<0.349.0>', '<0.338.0>', 1523879339251087, 1523879339251631).
% proc('<0.462.0>', '<0.337.0>', 1523890092344224, 1523890997994177).
% proc('<0.347.0>', '<0.338.0>', 1523879339250870, 1523879339251475).
% proc('<0.340.0>', '<0.337.0>', 1523879032969419, 1523879339249661).
% proc('<0.361.0>', '<0.357.0>', 1523879339252449, 1523879339252627).
% proc('<0.356.0>', '<0.338.0>', 1523879339251844, 1523879339252365).
% proc('<0.344.0>', '<0.343.0>', 1523879339250460, 1523879339251191).
% proc('<0.365.0>', '<0.337.0>', 1523879476085525, 1523886011086283).
% proc('<0.363.0>', '<0.337.0>', 1523879339252802, 1523879476085322).
% proc('<0.352.0>', '<0.347.0>', 1523879339251458, 1523879339251713).
% proc('<0.357.0>', '<0.338.0>', 1523879339251995, 1523879339252462).
% proc('<0.353.0>', '<0.348.0>', 1523879339251575, 1523879339251600).
% proc('<0.432.0>', '<0.337.0>', 1523887228942364, 1523890051919494).
% proc('<0.345.0>', '<0.338.0>', 1523879339250517, 1523879339251336).
% proc('<0.351.0>', '<0.346.0>', 1523879339251412, 1523879339251448).
% proc('<0.452.0>', '<0.450.0>', 1523889301164497, 1523889301164603).
% 
% proc('<0.337.0>', '<0.335.0>', 1523879028574485, 9999999999999999).
% proc('<0.472.0>', '<0.337.0>', 1523891023188425, 9999999999999999).
% 
% proc('<0.450.0>', nil, 1523889301164358, 1523889301270032).
% proc('<0.338.0>', '<0.337.0>', 1523879028578807, 9999999999999999).

spawn('<0.337.0>','<0.461.0>').
spawn('<0.356.0>','<0.360.0>').
spawn('<0.337.0>','<0.470.0>').
spawn('<0.338.0>','<0.355.0>').
spawn('<0.337.0>','<0.459.0>').
spawn('<0.338.0>','<0.346.0>').
spawn('<0.338.0>','<0.348.0>').
spawn('<0.338.0>','<0.358.0>').
spawn('<0.335.0>','<0.337.0>').
spawn('<0.338.0>','<0.343.0>').
spawn('<0.337.0>','<0.421.0>').
spawn('<0.345.0>','<0.350.0>').
spawn('<0.349.0>','<0.354.0>').
spawn('<0.337.0>','<0.339.0>').
spawn('<0.358.0>','<0.362.0>').
spawn('<0.355.0>','<0.359.0>').
spawn('<0.338.0>','<0.349.0>').
spawn('<0.337.0>','<0.462.0>').
spawn('<0.338.0>','<0.347.0>').
spawn('<0.337.0>','<0.340.0>').
spawn('<0.337.0>','<0.472.0>').
spawn('<0.357.0>','<0.361.0>').
spawn('<0.338.0>','<0.356.0>').
spawn('<0.343.0>','<0.344.0>').
spawn('<0.337.0>','<0.365.0>').
spawn('<0.337.0>','<0.363.0>').
spawn('<0.347.0>','<0.352.0>').
spawn('<0.338.0>','<0.357.0>').
spawn('<0.348.0>','<0.353.0>').
spawn('<0.337.0>','<0.432.0>').
spawn('<0.338.0>','<0.345.0>').
spawn('<0.346.0>','<0.351.0>').
spawn('<0.450.0>','<0.452.0>').
spawn('<0.337.0>','<0.338.0>').



% 
% proc('<1>', 1, 1).
% proc('<2>', 2, 2).
% proc('<3>', 1, 5).
% proc('<4>', 3, 7).
% proc('<5>', 4, 9).
% proc('<6>', 6, 10).
% proc('<7>', 2, 8).
% 
% % spawn(Parent, Child)
% spawn('<1>', '<3>').
% spawn('<2>', '<7>').
% spawn('<3>', '<4>').
% spawn('<3>', '<5>').
% spawn('<4>', '<6>').

ancestor(Pid1, Pid2) :-
  spawn(Pid1, Pid2).

ancestor(Pid1, Pid2) :-
  spawn(Pid1, Child),
  ancestor(Child, Pid2).



% we don't allow order where spawn happens from right-to-left,
% only from left to right
correct_order([_]).
correct_order([Pid1, Pid2 | Pids]) :-
  \+ ancestor(Pid2, Pid1),
  correct_order([Pid1 | Pids]),
  correct_order([Pid2 | Pids]).



% not1(Goal) :-
%   Goal,
%   !,
%   fail.



% iteratively try every element from unordered list
% and take it as first
find_order([X], [X]).
find_order(AnyOrder, CorrectOrder) :-
  find_order(AnyOrder, AnyOrder, CorrectOrder).



find_order([], _, _) :-
  fail.

find_order([E | Elements], AnyOrder, CorrectOrder) :-
  % remove_from_list(E, AnyOrder, AnyOrder1), % without E element
  select(E, AnyOrder, AnyOrder1), % without E element
  find_order(AnyOrder1, CorrectOrder1),
  CorrectOrder = [E | CorrectOrder1],
  correct_order(CorrectOrder);

  find_order(Elements, AnyOrder, CorrectOrder).


remove_from_list(_, [], []).
remove_from_list(E, [E | OldList], NewList) :-
  remove_from_list(E, OldList, NewList).

remove_from_list(E, [A | OldList], [A | NewList]) :-
  E \== A,
  remove_from_list(E, OldList, NewList).

% ['<1>', '<2>', '<3>', '<4>', '<5>', '<6>', '<7>']
% ['<2>', '<6>', '<7>', '<1>', '<3>', '<4>', '<5>']

% pids(L) :-
%   findall(P, proc(P,_,_), L).

% ['<0.450.0>', '<0.461.0>', '<0.337.0>', '<0.360.0>', '<0.356.0>', '<0.470.0>', '<0.355.0>', '<0.338.0>', '<0.459.0>', '<0.346.0>', '<0.348.0>', '<0.358.0>', '<0.335.0>', '<0.343.0>', '<0.421.0>', '<0.350.0>', '<0.345.0>', '<0.354.0>', '<0.349.0>', '<0.339.0>', '<0.362.0>', '<0.359.0>', '<0.462.0>', '<0.347.0>', '<0.340.0>', '<0.472.0>', '<0.361.0>', '<0.357.0>', '<0.344.0>', '<0.365.0>', '<0.363.0>', '<0.352.0>', '<0.353.0>', '<0.432.0>', '<0.351.0>', '<0.452.0>']