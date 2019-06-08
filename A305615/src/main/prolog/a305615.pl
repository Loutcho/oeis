main :- a(100, A, _, _), reverse(A, R), write_term(R, [spacing(next_argument)]).
a(0, [0], [0], []) :- !.
a(N, A, V, P) :-
  M is N - 1, a(M, AA, VV, PP), AA = [AM | _],
  findall(L, (member(L, VV), not(member([AM, L], PP))), Ls),
  (Ls = [L | _] -> V = VV ; (length(VV, L),V = [L | VV])),
  A = [L | AA], P = [[AM, L] | PP].
