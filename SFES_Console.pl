% Author: Wembley Williams, Mikhail Shaw
% Date: 10/27/2017

:- dynamic patient_sym/1, prevention/1, p_drugs/1.

%Initial patient_sym and p_drugs facts for the dynamic databases
patient_sym('').
p_drugs('').

%Full Blown Swine Flu symptoms attract a risk tally (RT) of 160

%To remove all the previous patient/s symptoms
undoSym :- retract(patient_sym(_)),fail.
undoDrugs :- retract(p_drugs(_)),fail.

%Drug database for educated assumptions for relief
drugFor(advil,headache).
drugFor(mucinex,cough).
drugFor(eye_drops,watery_red_eyes).
drugFor(tylenol,sore_throat).
drugFor(rest,fatigue).
drugFor(nytol,runny_nose).
drugFor(motrin,body_aches).
drugFor(xanax,nausea).
drugFor(pepto_bismal,diarrhea).
drugFor(imodium,vomiting).
drugFor(aspirin,fever).

%Database of swine flu symptoms for quick analysis and elimination
symptom(RL5,cough)          :- write('Is you coughing my guy? (y/n)            : '), read(Ans), Ans == y -> RL5  is  5,asserta(patient_sym(cough)); RL5  is 0.
symptom(RL7,watery_red_eyes):- write('Eyes all watery and red? (y/n)           : '), read(Ans), Ans == y -> RL7  is 10,asserta(patient_sym(watery_red_eyes)); RL7  is 0.
symptom(RL3,sore_throat)    :- write('Throat all sore? (y/n)                   : '), read(Ans), Ans == y -> RL3  is  5,asserta(patient_sym(sore_throat)); RL3  is 0.
symptom(RL4,fatigue)        :- write('Been feeling really tired lately? (y/n)  : '), read(Ans), Ans == y -> RL4  is 20,asserta(patient_sym(fatigue)); RL4  is 0.
symptom(RL6,runny_nose)     :- write('Is your nose running and stuffy? (y/n)   : '), read(Ans), Ans == y -> RL6  is 10,asserta(patient_sym(runny_nose)); RL6  is 0.
symptom(RL8,body_aches)     :- write('Do you experience body aches? (y/n)      : '), read(Ans), Ans == y -> RL8  is 20,asserta(patient_sym(body_aches)); RL8  is 0.
symptom(RL2,headache)       :- write('Killer headache? (y/n)                   : '), read(Ans), Ans == y -> RL2  is 20,asserta(patient_sym(headache)); RL2  is 0.
symptom(RL10,nausea)        :- write('Do you experience nausea? (y/n)          : '), read(Ans), Ans == y -> RL10 is 20,asserta(patient_sym(nausea)); RL10 is 0.
symptom(RL11,vomiting)      :- write('Do you experience vomiting? (y/n)        : '), read(Ans), Ans == y -> RL11 is 20,asserta(patient_sym(vomiting)); RL11 is 0.
symptom(RL9,diarrhea)       :- write('Do you have diarrhea? (y/n)              : '), read(Ans), Ans == y -> RL9  is 30,asserta(patient_sym(diarrhea)); RL9  is 0.

%Database of facts to check if a female patient is pregnant
p_symptom(PL0,cravings)       :- write('Have you been craving odd food ? out of your diet ?    : '), read(Ans), Ans == y -> PL0 is 5.
p_symptom(PL1,mood_swings)    :- write('Have you been swinging moods ?                         : '), read(Ans), Ans == y -> PL1 is 5.
p_symptom(PL2,period)         :- write('Was your last period more than 3 weeks ago ?           : '), read(Ans), Ans == y -> PL2 is 20.
p_symptom(PL3,tender_breasts) :- write('Are your breasts a bit tender ?                        : '), read(Ans), Ans == y -> PL3 is 5.
p_symptom(PL4,slight_bleeding):- write('Been having frequent headaches ?                       : '), read(Ans), Ans == y -> PL4 is 5.

%Database of prevention methods
prevention('Stay home if you are feeling sick ').
prevention('Wash your hands thoroughly and frequently ').
prevention('Contain your coughs and sneezes ').
prevention('Avoid contact. ').
prevention('Reduce exposure within your household. ').

%Predicate used to present a drug for each system
get_drug(X,Y) :- drugFor(X,Y),patient_sym(Y),asserta(p_drugs(X)),
                 format('The drug ~q was prescribed for your ~q',[X,Y]),nl,fail.

%Allows the patient to add suggestive prevention means
update_prevention(ToPrevent) :- write('Type your suggestion'),nl,
                                read(ToPrevent),
                                asserta(prevention(ToPrevent)),
                                write('Prevention measure added!'),nl,
                                write('Add another ?(y/n)'),
                                read(V),
                                (V == y -> update_prevention(ToPrevent),!;write('Kay.'),nl,menu).
                                
update_drugs(Drug,Ailment) :- write('Enter the new drug'),nl,
                              read(Drug),
                              write('And what does it help with ?'),nl,
                              read(Ailment),
                              asserta(drugFor(Drug,Ailment)),
                              format('So, ~q is for ~q ',[Drug,Ailment]),nl,
                              write('New drug added!'),nl,
                              write('Add another ?(y/n)'),
                              read(S),
                              (S == y -> update_drugs(Drug,Ailment),!;write('Kay.'),nl,menu).

%Tallying the risk of pregnancy which increases the risk of swine flu
p_symptom_tally(PT) :- p_symptom(PL0,cravings),
                       p_symptom(PL1,mood_swings),
                       p_symptom(PL2,period),
                       p_symptom(PL3,tender_breasts),
                       p_symptom(PL4,slight_bleeding),
                       (PT is PL0 + PL1 + PL2 + PL3 + PL4),
                       write('Pregnancy Tally -> '), write(PT).

%Tallying the symptoms of swine flu present in the patient
symptom_tally(RT) :-  symptom(RL5,cough),
                      symptom(RL2,headache),
                      symptom(RL3,sore_throat),
                      symptom(RL4,fatigue),
                      symptom(RL6,runny_nose),
                      symptom(RL7,watery_red_eyes),
                      symptom(RL8,body_aches),
                      symptom(RL9,diarrhea),
                      symptom(RL10,nausea),
                      symptom(RL11,vomiting),
                      (RT is RL2 + RL3 + RL4 + RL5 + RL6 + RL7 + RL8 + RL9 + RL10 + RL11),
                      write('Risk Tally -> '), write(RT).

%To show all prevention suggestions
toPrevent(P) :- prevention(P),write(P),nl,fail.

%To show all patient symptoms
getPatientSymptoms(PS) :- patient_sym(PS),write(PS),write(' '),fail.

%To show all drugs prescribed to patient
getPatientDrugs(PD) :- p_drugs(PD),write(PD),write(' '),fail.

%Simple menu to prompt the user
menu :- write('----------------------------------------------'),nl,
        write('                    MENU                      '), nl,
        write('----------------------------------------------'),nl,
        write('               1. H1N1 Diagnosis              '),nl,
        write('----------------------------------------------'),nl,
        write('         2. Update Prevention Database        '),nl,
        write('----------------------------------------------'),nl,
        write('            3. Update Drug Database           '),nl,
        write('----------------------------------------------'),nl,
        write('                  4. Exit                     '),nl,
        write('----------------------------------------------'),nl,
        read(Choice),
        (Choice == 1
        -> survey,undoSym,undoDrugs;
        Choice == 2
        -> update_prevention(ToPrevent);
        Choice == 3
        -> update_drugs(Drug,Ailment);
        Choice == 4
        -> write('Okay');
        write('Choice Invalid'),menu).


%Summary to show final report of current patient
infected_summary(Firstname,Lastname,Gender,Age,Temp,Con,A,Drugs) :- sleep(0.5),
                                                   write('Hello '),
                                                   write(Firstname),write(' '),write(Lastname),nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('                  SUMMARY                     '), nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('Gender                  : '),
                                                   (Gender == m -> write('Male');write('Female')),nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('Age                     : '),write(Age),nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('Recorded Temperature(F) : '),write(Temp),nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('H1N1 Diagnosis          : '),write(Con),nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('Symptoms                : '),getPatientSymptoms(A),fail,nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('Prescribed drugs        : '),getPatientDrugs(Drugs),fail,nl,
                                                   write('----------------------------------------------'),nl.
summary(Firstname,Lastname,Gender,Age,Temp,Con) :- sleep(0.5),
                                                   write('Hello '),
                                                   write(Firstname),write(' '),write(Lastname),nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('                  SUMMARY                     '), nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('Gender                  : '),
                                                   (Gender == m -> write('Male');write('Female')),nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('Age                     : '),write(Age),nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('Recorded Temperature(F) : '),write(Temp),nl,
                                                   write('----------------------------------------------'),nl,
                                                   write('H1N1 Diagnosis          : '),write(Con),nl,
                                                   write('----------------------------------------------'),nl.

%A simple splashscreen to greet the user :)
simplesplash :-
       sleep(0.4),
       write('---------------------------------------------------------------------------'),nl,
       sleep(0.4),
       write('*************************'),nl,
       sleep(0.2),
       write('###################||| SWINE FLU EXPERT SYSTEM |||#########################'),nl,
       sleep(0.4),
       write('*************************'),nl,
       sleep(0.4),
       write('---------------------------------------------------------------------------'),nl,nl,nl.

survey :-
       simplesplash,

       write('Please enter your first name                 : '),read(Firstname), nl,
       write('Ah, now enter your last name                 : '),read(Lastname), nl,
       write(' are you male or female ?(m/f)               : '),read(Gender), nl,
       write(' Mad, how old are you ?                      : '),read(Age), nl,
       write('Awesome :), give us your body temp (celsius) : '),read(Temp), nl,

       %Calculating the patients temperature by converting from celsius degrees to fahrenheit
       FTemp is Temp * 1.8 + 32, nl,
       (FTemp > 100.4 -> TR is 20,write('You have a fever'),asserta(patient_sym(fever));
       TR is 0,
       write('Your temperature is of no concern')),nl,nl,
       
       write('Thanks For The Info, '),write(Firstname), nl,nl,
       sleep(0.4),
       write('---------------------------------------------------------------------------'),nl,
       write('                           Processing...                                   '), nl,
       write('---------------------------------------------------------------------------'),nl,
       sleep(0.2),
       write('Now lets ask a few questions.'),sleep(0.1), nl,
       symptom_tally(RT),
       nl,

       write('Thank You'), nl,

       %Pregnancy is highly unlikely between this age range
       (Gender == f,Age > 10, Age < 45  -> write('Since you are indeed female, we will need to ask some additional questions '), nl
       ,p_symptom_tally(PT);PT is 0),
       
       TotalRisk is RT + PT + TR,
       write('Your Risk Level is -> '),
       write(TotalRisk), nl,nl,

       (TotalRisk < 50 ->
       Condition = 'Not Infected',
       summary(Firstname,Lastname,Gender,Age,FTemp,Condition),
       write('Here is some advice '), nl,nl,nl,
       toPrevent(P),nl;
       TotalRisk >= 90 ->
       write('You are at risk !'), nl,
       get_drug(M,N),!,
       Condition = 'At Risk'),
       write('We will suggest the following: '),nl,
       infected_summary(Firstname,Lastname,Gender,Age,FTemp,Condition,A,Drugs);
       
       menu.