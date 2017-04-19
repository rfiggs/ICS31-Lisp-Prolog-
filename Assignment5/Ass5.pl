/* NOTE: This file is just a starting point! It is very much incomplete. You will have to modify the given clauses, and add new ones.

Try these:

?- top([is, it, true, that, mark, hamill, acts, in, star, wars, iv]).

?- top([who, acts, in, star, wars, iv]).

top([is, it, true, that, frank, oz, plays, yoda]).
top([is, it, true, that, frank, oz, is, a, director]).
top([is, it, true, that, frank, oz, is, an, actor]).
top([did, frank, oz, play, yoda, in, star, wars, i]).
top([is, it, true, that, frank, oz, plays, yoda]).
top([is, it, true, that, frank, oz, plays, yoda, in, star, wars, i]).
top([who, is, the, actor, for, sebulba]).
top([who, is, the, director, of, star, wars, i]).
top([who, is, the, character, of, frank, oz]).
top([what, is, the, title, of, star, wars, i]).
top([george,lucas,directed,star,wars,i,right]).
top([george,lucas,directed,star,wars,i,and,natalie,portman,plays,padme,right]).
top([george,lucas,directed,star,wars,i,lewis,macleod,plays,sebulba,and,natalie,portman,plays,padme,right]).
top([george,lucas,is,a,director,lewis,macleod,is,an,actor,and,natalie,portman,plays,padme,right]).
top([is,it,true,that,frank,oz,plays,yoda,and,natalie,portman,plays,padme]).
top([is,it,true,that,frank,oz,plays,yoda,lewis,macleod,plays,sebulba,and,natalie,portman,plays,padme]).
*/

/* TOP/1

ARG1 is a sentence represented as a list of atoms (e.g. [is, it, true, that, mark, hamill, acts, in, star, wars, iv]).

TOP/1 will succeed or fail. Either way, it should write out a sensible answer.

*/
writeName([H]):-
    write(H).
writeName([H|T]):-
    format("~w ",H),
    writeName(T).

top(Sentence) :-
  yesno(Query, Sentence, []), % This is a call to the DCG.
  showresults(Query).

top(Sentence) :-
  who(Who, Sentence, []), % This is a call to the DCG.
  write("The person you're looking for is "),
  name(Who,Name),
  writeName(Name). % Can you make this better? It really should write out the text of the answer, not just the symbol.



top(_) :-
    write("I don't get it").

/* SHOWRESULTS/1 writes out positive text if ARG1 is a list of true predicates, negative text otherwise. */
showresults(Query) :-
  test(Query),
  write("Yes, that's true.").

showresults(_) :-
  write("Sorry, that's false.").

/* TEST/1 takes a list of predicates, and succeeds if all the predicates are true, otherwise fails.*/

test([Query]) :-
  Query.

test([Query|Rest]):-
  Query,
  test(Rest).

/* DCG: Here's the grammar. Right now it's very simple. */


who(X) --> [who, is], query_statement(X^_^_^[Query],[type=who]), {Query}.
who(X) --> [what, is], query_statement(X^_^_^[Query],[type=what]), {Query}.

yesno(Sem) --> [is, it, true, that], statement(_^_^_^Sem,[type=right]).

yesno(Sem) --> statement(_^_^_^Sem,[type=right]), [right].

yesno(Sem) --> [did], statement(_^_^_^Sem,[type=did]).

statement(S,[type=Type]) --> singlestatement(S,[type=Type]).
statement(_^_^_^Sem,[type=Type]) -->
    singlestatement(_^_^_^S1,[type=Type]),
    [and],
    statement(_^_^_^S2,[type=Type]),
        {append(S1,S2,Sem)}.
statement(_^_^_^Sem,[type=Type]) -->
    singlestatement(_^_^_^S1,[type=Type]),
    statement(_^_^_^S2,[type=Type]),
        {append(S1,S2,Sem)}.


singlestatement(Subj^Obj^Sem,[type=Type]) -->
 noun_phrase(Subj),
 verb_phrase(Subj^Obj^Sem,[type=Type]).

 singlestatement(Subj^Obj^IndObj^Sem,[type=Type]) -->
  noun_phrase(Subj),
  verb_phrase(Subj^Obj^IndObj^Sem,[type=Type]).

verb_phrase(Subj^Obj^_^Sem,[type=Type]) -->
  verb(Subj^Obj^Sem,[type=Type]),
  noun_phrase(Obj).

verb_phrase(Subj^Obj^IndObj^Sem,[type=Type]) -->
  verb(Subj^Obj^IndObj^Sem,[type=Type]),
  noun_phrase(Obj),
  [in],
  noun_phrase(IndObj).

query_statement(Subj^Obj^_^Sem,[type=Type]) -->
    noun_phrase(IndObj),
    query_verb_phrase(Subj^Obj^IndObj^Sem,[type=Type]).

query_verb_phrase(Subj^Obj^IndObj^Sem,[type=Type]) -->
    verb(Subj^Obj^IndObj^Sem,[type=Type]),
    noun_phrase(Obj).

noun_phrase(Sem) --> proper_noun(Sem).
noun_phrase(Sem) --> det([vowel=Vowel]), noun(Sem,[vowel=Vowel]).

det([vowel=y]) --> [an].
det([vowel=n]) --> [a].
det([vowel=_]) --> [the].

noun(director,[vowel=n]) --> [director].
noun(character,[vowel=n]) --> [character].
noun(actor,[vowel=y]) --> [actor].
noun(title,[vowel=n]) --> [title].



% proper_noun(mark_hamill) --> [mark, hamill].
% proper_noun(harrison_ford) --> [harrison, ford].
% proper_noun(star_wars4) --> [star, wars, iv].
% proper_noun(star_wars3) --> [star, wars, iii].
% proper_noun(luke_skywalker) --> [luke, skywalker].
verb(X^Y^[plays(X,Y,_)],[type=right]) --> [plays].
verb(X^Y^Z^[plays(X,Y,Z)],[type=right]) --> [plays].
verb(X^Y^[directs(X,Y)],[type=right]) --> [directed].
verb(X^director^[director(X)],[type=right]) --> [is].
verb(X^actor^[actor(X)],[type=right]) --> [is].
verb(X^character^[character(X)],[type=right]) --> [is].
verb(X^film^[film(X)],[type=right]) --> [is].

verb(X^Y^[plays(X,Y,_)],[type=did]) --> [play].
verb(X^Y^[directs(X,Y)],[type=did]) --> [direct].
verb(X^Y^Z^[plays(X,Y,Z)],[type=did]) --> [play].

verb(X^Y^actor^[plays(X,Y,_)],[type=who]) --> [for].
verb(X^Y^director^[directs(X,Y)],[type=who]) --> [of].
verb(X^Y^character^[plays(Y,X,_)],[type=who]) --> [of].

verb(X^Y^title^[title(Y,X)],[type=what]) --> [of].

actor(X) :-
    plays(X,_,_).
director(X):-
    directs(X,_).
character(X):-
    plays(_,X,_).
film(X):-
    title(X,_);
    directs(X,_);
    plays(_,_,X).

/* DATABASE. Obviously, you're going to have to fill this out a lot. */

/* Star Wars I*/
title(star_wars1,[the, phantom, menace]).
directs(george_lucas, star_wars1).
plays(liam_neeson, qui_gon_jinn, star_wars1).
plays(ewan_mcgregor, obi_wan_kenobi, star_wars1).
plays(natalie_portman, queen_amidala, star_wars1).
plays(natalie_portman, padme, star_wars1).
plays(jake_lloyd, anakin_skywalker, star_wars1).
plays(ian_mcdiarmid, senator_palpatine, star_wars1).
plays(anthony_daniels, c_3po, star_wars1).
plays(frank_oz, yoda, star_wars1).
plays(ray_park, darth_maul, star_wars1).
plays(lewis_macleod, sebulba, star_wars1).
plays(samuel_l_jackson, mace_windu, star_wars1).


name(george_lucas,['George','Lucas']).
proper_noun(star_wars1) --> [star, wars, i].
proper_noun(george_lucas) --> [george, lucas].
proper_noun(liam_neeson) --> [liam, neeson].
proper_noun(qui_gon_jinn) --> [qui, gon, jinn].
proper_noun(ewan_mcgregor) --> [ewan, mcgregor].
proper_noun(obi_wan_kenobi) --> [obi, wan, kenobi].
proper_noun(natalie_portman) --> [natalie, portman].
proper_noun(padme) --> [padme].
proper_noun(queen_amidala) --> [queen, amidala].
proper_noun(jake_lloyd) --> [jake, lloyd].
proper_noun(anakin_skywalker) --> [anakin, skywalker].
proper_noun(ian_mcdiarmid) --> [ian, mcdirmid].
proper_noun(senator_palpatine) --> [senator, palpatine].
proper_noun(anthony_daniels) --> [anthony, daniels].
proper_noun(c_3po) --> [c3po].
proper_noun(frank_oz) --> [frank, oz].
proper_noun(yoda) --> [yoda].
proper_noun(ray_park) --> [ray, park].
proper_noun(darth_maul) --> [darth, maul].
proper_noun(lewis_macleod) --> [lewis, macleod].
proper_noun(sebulba) --> [sebulba].
proper_noun(samuel_l_jackson) --> [sameul, l, jackson].
proper_noun(mace_windu) --> [mace, windu].

/*
actor(mark_hamill).
actor(harrison_ford).
acts_in(mark_hamill, star_wars4).
acts_in(harrison_ford, star_wars4).
plays(mark_hamill, luke_skywalker).
*/
proper_noun(X) --> {name(X,N)},N.
