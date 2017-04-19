/* NOTE: This file is just a starting point! It is very much incomplete. You will have to modify the given clauses, and add new ones.

Try these:

top([is, it, true, that, frank, oz, plays, yoda]).
top([is, it, true, that, idris, elba, is, an, actor]).
top([did, chris, pine, play, kirk, in, star, trek, i]).
top([who, is, the, actor, for, sebulba]).
top([who, is, the, director, of, star, trek, iii]).
top([who, is, the, character, of, benedict,cumberbatch]).
top([what, is, the, title, of, star, trek, ii]).
top([george,lucas,directed,star,wars,i,and,natalie,portman,plays,padme,right]).
top([george,lucas,directed,star,wars,i,chris,pine,plays,kirk,and,sofia,boutella,plays,jaylah,right]).
top([george,lucas,is,a,director,lewis,macleod,is,an,actor,and,natalie,portman,plays,padme,right]).
top([is,it,true,that,justin,lin,is,a,director,lewis,macleod,plays,sebulba,and,natalie,portman,plays,padme]).

ungrammatical
top([did,george,lucas,plays,spock]).
top([is,it,true,that,spock,is,a,actor]).
top([liam,neeson,play,kahn,right]).
top([justin,lin,is,an,director]).

*/



% apend/3
% appends to lists together
% arg1, the first list to append
% arg2, the second list to append
% arg3, the two lists appeneded together
% at lesat one argument must be instantiated for this predicate to work
% from assignment 4
apend([], L2, L2).
apend([H1|T1], L2, [H1|L]):-
    apend(T1,L2,L).

% writeName/1
% prints out a name stored as a list
writeName([H]):-
    write(H).
writeName([H|T]):-
    format("~w ",H),
    writeName(T).

    /* TOP/1

    ARG1 is a sentence represented as a list of atoms (e.g. [is, it, true, that, mark, hamill, acts, in, star, wars, iv]).

    TOP/1 will succeed or fail. Either way, it should write out a sensible answer.

    */
top(Sentence) :-
  yesno(Query, Sentence, []),
  showresults(Query).

top(Sentence) :-
  who(Who, Sentence, []),
  write("The person you're looking for is "),
  name(Who,Name),
  writeName(Name).

top(Sentence) :-
    what(What, Sentence, []),
    writeName(What).

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


who(X) --> [who, is], query_statement(X^_^_^[Query],[type=who]), {Query}.
what(X) --> [what, is], query_statement(X^_^_^[Query],[type=what]), {Query}.

yesno(Sem) --> [is, it, true, that], statement(_^_^_^Sem,[type=right]).

yesno(Sem) --> statement(_^_^_^Sem,[type=right]), [right].

yesno(Sem) --> [did], statement(_^_^_^Sem,[type=did]).

statement(S,[type=Type]) --> singlestatement(S,[type=Type]).
statement(_^_^_^Sem,[type=Type]) -->
    singlestatement(_^_^_^S1,[type=Type]),
    [and],
    statement(_^_^_^S2,[type=Type]),
        {apend(S1,S2,Sem)}.
statement(_^_^_^Sem,[type=Type]) -->
    singlestatement(_^_^_^S1,[type=Type]),
    statement(_^_^_^S2,[type=Type]),
        {apend(S1,S2,Sem)}.


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

proper_noun(X) --> {name(X,N)},N.
/* DATABASE */

title(star_wars1, [the, phantom, menace]).
title(star_wars2, [attack, of, the, clones]).
title(star_wars3, [revenge, of, the, sith]).
title(star_trek1, [star,trek]).
title(star_trek2, [star,trek,into,darkness]).
title(star_trek3, [star,trek,beyond]).

directs(george_lucas, star_wars1).
directs(george_lucas, star_wars2).
directs(george_lucas, star_wars3).
directs(jj_abrams, star_trek1).
directs(jj_abrams, star_trek2).
directs(justin_lin, star_trek3).
/* Star Wars I*/

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

/*Star Wars II*/

plays(ewan_mcgregor, obi_wan_kenobi, star_wars2).
plays(natalie_portman, padme, star_wars2).
plays(hayden_christensen, anakin_skywalker, star_wars2).
plays(christopher_lee, count_dooku, star_wars2).
plays(samuel_l_jackson, mace_windu, star_wars2).
plays(frank_oz, yoda, star_wars2).
plays(ian_mcdiarmid, supreme_chancellor_palpatine, star_wars2).
plays(pernilla_august, shmi_skywalker, star_wars2).
plays(temuera_morrison, jango_fett, star_wars2).
plays(jimmy_smits, senator_bail_organa, star_wars2).
plays(jack_thompson, cliegg_lars, star_wars2).

% Star Wars III

plays(ewan_mcgregor, obi_wan_kenobi, star_wars3).
plays(natalie_portman, padme, star_wars3).
plays(hayden_christensen, anakin_skywalker, star_wars3).
plays(ian_mcdiarmid, supreme_chancellor_palpatine, star_wars3).
plays(samuel_l_jackson, mace_windu, star_wars3).
plays(jimmy_smits, senator_bail_organa, star_wars3).
plays(frank_oz, yoda, star_wars3).
plays(anthony_daniels, c_3po, star_wars3).
plays(christopher_lee, count_dooku, star_wars3).
plays(peter_mahew, chewbacca, star_wars3).
plays(keisha_castle_hughes, queen_naboo, star_wars3).


% Stark Trek 1

plays(chris_pine, kirk, star_trek1).
plays(zachary_quinto, spock, star_trek1).
plays(leonard_nimoy, spock_prime, star_trek1).
plays(eric_bana, nero, star_trek1).
plays(bruce_greenwood, pike, star_trek1).
plays(karl_urban, bones, star_trek1).
plays(zoe_saldana, uhura, star_trek1).
plays(simon_pegg, scotty, star_trek1).
plays(john_cho, sulu, star_trek1).
plays(anton_yelchin, chekov, star_trek1).
plays(ben_cross, sarek, star_trek1).

%Star Trek 2

plays(chris_pine, kirk, star_trek2).
plays(zachary_quinto, spock, star_trek2).
plays(zoe_saldana, uhura, star_trek2).
plays(karl_urban, bones, star_trek2).
plays(simon_pegg, scotty, star_trek2).
plays(john_cho, sulu, star_trek2).
plays(benedict_cumberbatch, khan, star_trek2).
plays(anton_yelchin, chekov, star_trek2).
plays(bruce_greenwood, pike, star_trek2).
plays(peter_weller, marcus, star_trek2).
plays(alice_eve, carol_marcus, star_trek2).

% star trek 3

plays(chris_pine, kirk, star_trek3).
plays(zachary_quinto, spock, star_trek3).
plays(karl_urban, bones, star_trek3).
plays(zoe_saldana, uhura, star_trek3).
plays(simon_pegg, scotty, star_trek3).
plays(john_cho, sulu, star_trek3).
plays(anton_yelchin, chekov, star_trek3).
plays(idris_elba, krall, star_trek3).
plays(sofia_boutella, jaylah, star_trek3).
plays(joe_taslim, manas, star_trek3).
plays(lydia_wilson, kalara, star_trek3).

name(star_wars1, [star, wars, i]).
name(star_wars2, [star, wars, ii]).
name(star_wars3, [star, wars, iii]).
name(star_trek1, [star, trek, i]).
name(star_trek2, [star, trek, ii]).
name(star_trek3, [star, trek, iii]).
name(george_lucas,[george, lucas]).
name(george_lucas, [george, lucas]).
name(liam_neeson, [liam, neeson]).
name(qui_gon_jinn, [qui, gon, jinn]).
name(ewan_mcgregor, [ewan, mcgregor]).
name(obi_wan_kenobi, [obi, wan, kenobi]).
name(natalie_portman, [natalie, portman]).
name(padme, [padme]).
name(queen_amidala, [queen, amidala]).
name(jake_lloyd, [jake, lloyd]).
name(anakin_skywalker, [anakin, skywalker]).
name(ian_mcdiarmid, [ian, mcdiarmid]).
name(senator_palpatine, [senator, palpatine]).
name(anthony_daniels, [anthony, daniels]).
name(c_3po, [c3po]).
name(frank_oz, [frank, oz]).
name(yoda, [yoda]).
name(ray_park, [ray, park]).
name(darth_maul, [darth, maul]).
name(lewis_macleod, [lewis, macleod]).
name(sebulba, [sebulba]).
name(samuel_l_jackson, [sameul, l, jackson]).
name(mace_windu, [mace, windu]).
name(hayden_christensen, [hayden, christensen]).
name(christopher_lee, [christopher, lee]).
name(count_dooku, [count, dooku]).
name(supreme_chancellor_palpatine, [supreme, chancellor, palpatine]).
name(pernilla_august, [pernilla, august]).
name(shmi_skywalker, [shmi, skywalkerp]).
name(temuera_morrison, [temuera, morrison]).
name(jango_fett, [jango, fett]).
name(jimmy_smits, [jimmy, smits]).
name(senator_bail_organa, [senator, bail, organa]).
name(jack_thompson, [jack, thompson]).
name(cliegg_lars, [cliegg, lars]).
name(peter_mahew, [peter, mahew]).
name(chewbacca, [chewbacca]).
name(keisha_castle_hughes, [keisha, castle, hughes]).
name(queen_naboo, [queen, of, naboo]).

name(jj_abrams,[jj, abrams]).
name(justin_lin,[justin,lin]).
name(chris_pine,[chris, pine]).
name(kirk,[kirk]).
name(zachary_quinto,[zachary, quinto]).
name(spock,[spock]).
name(leonard_nimoy,[leonard,nimoy]).
name(spock_prime,[spock, prime]).
name(eric_bana,[eric, bana]).
name(nero, [nero]).
name(bruce_greenwood, [bruce, greenwood]).
name(pike, [pike]).
name(karl_urban, [karl,urban]).
name(bones, [bones]).
name(zoe_saldana, [zoe,saldana]).
name(uhura, [uhura]).
name(simon_pegg, [simon,pegg]).
name(scotty, [scotty]).
name(john_cho, [john,cho]).
name(sulu, [sulu]).
name(anton_yelchin, [anton,yelchin]).
name(chekov, [chekov]).
name(ben_cross, [ben,cross]).
name(sarek, [sarek]).
name(benedict_cumberbatch, [benedict,cumberbatch]).
name(khan, [khan]).
name(peter_weller, [peter,weller]).
name(marcus, [marcus]).
name(alice_eve, [alice,eve]).
name(carol_marcus, [carol,marcus]).
name(idris_elba, [idris,elba]).
name(krall, [krall]).
name(sofia_boutella, [sofia,boutella]).
name(jaylah, [jaylah]).
name(joe_taslim, [joe,taslim]).
name(manas, [manas]).
name(lydia_wilson, [lydia,wilson]).
name(kalara, [kalara]).
