% Author: Wembley Williams, Mikhail Shaw
% Date: 10/27/2017

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_path)).
:- style_check(-singleton).

%Full Blown Swine Flu symptoms attract a risk tally (RT) of 190
/*
Data.name,
                    Data.age,
                    Data.gender,
                    Data.temp,
                    Data.cough,
                    Data.eyes,
                    Data.fever,
                    Data.headache,
                    Data.throat,
                    Data.fatigue,
                    Data.nose,
                    Data.ache,
                    Data.nausea,
                    Data.vomitting,
                    Data.sActive,
                    Data.diarrhea
*/

:- dynamic patient_sym/1, prevention/1, p_drugs/1, patients/1,infected_patients/1,risk_patients/1.

%Initial facts for the dynamic databases
patient_sym().
p_drugs().
patients().
infected_patients().
risk_patients().

%Full Blown Swine Flu symptoms attract a risk tally (RT) of 160

%To remove all the previous patient/s symptoms
undoSym(X) :- forall(patient_sym(X),retract(patient_sym(X))).
undoDrugs(X) :- forall(p_drugs(X),retract(p_drugs(X))).

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

%Count the amount of patients asserted as overall patients healthy and all
total_patients(K) :- findall(X,patients(_),L),length(L,K).

%Count the amount of patients asserted as infected
total_infected_patients(T) :- findall(X,infected_patients(_),L),length(L,T).

%Count the amount of patients asserted as infected
total_risk_patients(F) :- findall(X,risk_patients(_),L),length(L,F).

%PErcentage of infected patients
percentage_of_infected(G,H,O) :- total_patients(G),total_infected_patients(H),
                                 O is (G * H) / 100.

%Print the appropriate drug for patients symptoms
test_gen:-
            phrase( gen_drug_ailment_data(cough), CoughHtml, []),print_html(CoughHtml).

get_drug_data(X, tr([ td(Y), td(X)])) :-
                    drugFor(X,Y).



print_drug_ailment(Ailment) -->{
    findall(X,drugFor(X,Ailment),Drugs),
    maplist(get_drug_data, Drugs, Data)
},
html([\html_post(drugData, Data)]).


gen_drug_ailment_data(Ailment) -->
        html([
            span([
                \print_drug_ailment(Ailment)          
            ]),
            span([
                \html_receive(drugData)
            ])
        ]).

getDrug(Data) :- 
                format('<h5>Prescribed Drugs</h5>
                        <table class="striped drug_table">
                            <thead>
                            <tr>
                                <th>Symptom</th>
                                <th>Drug</th>
                            </tr>
                            </thead>

                            <tbody>',[]),
                (Data.cough == 'yes' -> phrase( gen_drug_ailment_data(cough), CoughHtml, []),
                                        print_html(CoughHtml); format('',[])),
                (Data.headache == 'yes' -> phrase( gen_drug_ailment_data(headache), HeadacheHtml, []),
                                            print_html(HeadacheHtml); format('',[])),
                (Data.eyes == 'yes' -> phrase( gen_drug_ailment_data(watery_red_eyes), EyesHtml, []),
                                            print_html(EyesHtml); format('',[])),
                (Data.throat == 'yes' -> phrase( gen_drug_ailment_data(sore_throat), ThroatHtml, []),
                                            print_html(ThroatHtml); format('',[])),
                (Data.fatigue == 'yes' -> phrase( gen_drug_ailment_data(fatigue), FatigueHtml, []),
                                            print_html(FatigueHtml); format('',[])),
                (Data.nose == 'yes' -> phrase( gen_drug_ailment_data(runny_nose), NoseHtml, []),
                                            print_html(NoseHtml); format('',[])),
                (Data.ache == 'yes' -> phrase( gen_drug_ailment_data(body_aches), AcheHtml, []),
                                            print_html(AcheHtml); format('',[])),
                (Data.nausea == 'yes' -> phrase( gen_drug_ailment_data(nausea), NauseaHtml, []),
                                            print_html(NauseaHtml); format('',[])),
                (Data.diarrhea == 'yes' -> phrase( gen_drug_ailment_data(diarrhea), DiarrheaHtml, []),
                                            print_html(DiarrheaHtml); format('',[])),
                (Data.vomitting == 'yes' -> phrase( gen_drug_ailment_data(vomiting), VomitingHtml, []),
                                            print_html(VomitingHtml); format('',[])),
                (Data.fever == 'yes' -> phrase( gen_drug_ailment_data(fever), FeverHtml, []),
                                            print_html(FeverHtml); format('',[])),
                format('
                                
                            </tbody>
                        </table><br><br>
                ',[]).

%Database of swine flu symptoms for quick analysis and elimination
symptom(RL5, cough, Data)          :- Data.cough == yes -> RL5  is  5,asserta(patient_sym(cough)); RL5  is 0.
symptom(RL7, watery_red_eyes, Data):- Data.eyes == yes -> RL7  is 10,asserta(patient_sym(watery_red_eyes)); RL7  is 0.
symptom(RL1, fever, Data)          :- Data.fever == yes -> RL1  is 5,asserta(patient_sym(fever)); RL1  is 0.
symptom(RL3, sore_throat, Data)    :- Data.throat == yes -> RL3  is  5,asserta(patient_sym(sore_throat)); RL3  is 0.
symptom(RL4, fatigue, Data)        :- Data.fatigue == yes -> RL4  is 20,asserta(patient_sym(fatigue)); RL4  is 0.
symptom(RL6, runny_nose, Data)           :- Data.nose == yes -> RL6  is 10,asserta(patient_sym(runny_nose)); RL6  is 0.
symptom(RL8, body_aches, Data)     :- Data.ache == yes -> RL8  is 20,asserta(patient_sym(body_aches)); RL8  is 0.
symptom(RL2, headache, Data)       :- Data.headache == yes -> RL2  is 20,asserta(patient_sym(headache)); RL2  is 0.
symptom(RL10, nausea, Data)        :- Data.nausea == yes -> RL10 is 20,asserta(patient_sym(nausea)); RL10 is 0.
symptom(RL11, vomiting, Data)      :- Data.vomitting == yes -> RL11 is 20,asserta(patient_sym(vomiting)); RL11 is 0.
symptom(RL9, diarrhea, Data)       :- Data.diarrhea == yes -> RL9  is 30,asserta(patient_sym(diarrhea)); RL9  is 0.

%Database of facts to check if a female patient is pregnant
p_symptom(PL0, Data, food)       :- Data.food == yes -> PL0 is 5; PL0 is 0.
p_symptom(PL1, Data, mood)    :- Data.mood == yes -> PL1 is 5; PL1 is 0.
p_symptom(PL2, Data, headache)         :- Data.fheadache == yes -> PL2 is 5; PL2 is 0.
p_symptom(PL3, Data, breast) :- Data.breast == yes -> PL3 is 5; PL3 is 0.
p_symptom(PL4, Data, bleed):- Data.bleed == yes -> PL4 is 5; PL4 is 0.

%Database of prevention methods
prevention('Stay home if you are sick.').
prevention('Wash your hands thoroughly and frequently.').
prevention('Contain your coughs and sneezes.').
prevention('Avoid close contact.').
prevention('Reduce exposure within your household.').

%Allows the patient to add suggestive prevention means
suggestion(ToPrevent) :- asserta(prevention(ToPrevent)), write('Prevention measure added!').

%Tallying the risk of pregnancy which increases the risk of swine flu
p_symptom_tally(PT, Data) :- p_symptom(PL0, Data, food),
                       p_symptom(PL1, Data, mood),
                       p_symptom(PL2, Data, headache),
                       p_symptom(PL3, Data, breast),
                       p_symptom(PL4, Data, bleed),
                       PT is (PL0 + PL1 + PL2 + PL3 + PL4).
                       %write('Pregnancy Tally -> '), write(PT).



%Tallying the symptoms of swine flu present in the patient
symptom_tally(RT, Data) :-  symptom(RL1, cough, Data),
                      symptom(RL2, watery_red_eyes, Data),
                      symptom(RL3, fever, Data),
                      symptom(RL4, sore_throat,  Data),
                      symptom(RL5, fatigue, Data),
                      symptom(RL6, runny_nose, Data),
                      symptom(RL7, body_aches, Data),
                      symptom(RL8, headache, Data),
                      symptom(RL9, nausea, Data),
                      symptom(RL10, vomiting, Data),
                      symptom(RL11, diarrhea, Data),
                      (RT is RL1 + RL2 + RL3 + RL4 + RL5 + RL6 + RL7 + RL8 + RL9 + RL10 + RL11).
                      %write('Risk Tally -> '), write(RT).

main:- 
        format(' ~`-t~70| ~n'), %sleep(0.4),
        format(' ~`*t~70| ~n'), %sleep(0.4),
        format(' ~`#t~19|||| SWINE FLU EXPERT SYSTEM ||| ~`#t~70| ~n'), %sleep(0.4),
        format(' ~`*t~70| ~n'), %sleep(0.4),
        format(' ~`-t~70| ~n~n~n'),
        format('This Expert System was done as the backend of a web application. In order~n
to run said web application then run code:~n
>> server(Port).~n
*Replace Port with a port number~n
Next just open your broswer and navigate to http://localhost:Port/ and enjoy~n~n',[]).

server(Port) :-
        http_server(http_dispatch,
                    [ port(Port)
                    ]).


:-  http_handler('/', index, []),
    http_handler('/check',check,[]), 
    http_handler('/check/stat',checkStat,[]), 
    http_handler('/about',about,[]),
    http_handler('/pregCheck/Stat',pregStat,[]),
    http_handler('/check/temp',temp,[]),
    http_handler('/update/prevention',updatePrev,[]),
    http_handler('/update/drug',updateDrug,[]),
    http_handler('/stat', statusReport,[]),
    main.


head(X) :-
        X ='<head>
                        <title> SFES </title>   
                        <link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons">
                        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/css/materialize.min.css">
                        <script src="https://code.jquery.com/jquery-3.2.1.min.js" integrity="sha256-hwg4gsxgFZhOsEEamdOYGBf13FyQuiTwlAQgxVSNgt4=" crossorigin="anonymous"></script>
                        <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js"></script>
                        <link rel="shortcut-icon" href="/icon/favicon.ico"/>
                        <script language="javascript">
                             $(document).ready(function(){
                                 $(\'.modal\').modal();
                                /*var newHtml = $(\'#prevents\').text().replace("Content-type: text/html; charset=UTF-8","");
                                console.log(newHtml);
                                const newDiv = newHtml.split(".");
                                 $(\'#prevents\').empty();
                                 for(var x = 0;  x < newDiv.length - 1; x++){
                                     if(x == 0){
                                        var para = $("<li><h4>" + newDiv[x] + "</h4><hr class=\'grey lighten-5\'></li>");
                                        para.addClass("collection-header");                        
                                        $(\'#prevents\').append(para);
                                     }else{
                                        var para = $("<li>" + newDiv[x] + "</li>");
                                        para.addClass("collection-item");
                                        $(\'#prevents\').append(para);
                                     }
                                     
                                 }*/
                                 for(var x = 0; x < $(\'.drug_table td\').length; x++){
                                     $(\'.drug_table td\').eq(x).text($(\'.drug_table td\').eq(x).text().replace(/_/g," "));
                                 }
                                
                                 $(\'.progress\').css({"display":"none"});
                                /*document.getElementById("doNtn").addEventListener("click", function(e){
                                    e.preventDefault();
                                });
                                $(\'#doNtn\').click(function(e){
                                    e.preventDefault();
                                    alert("This does nuffink!!");
                                });*/
                                $(".button-collapse").sideNav();
                                $(window).on("beforeload", function(){
                                    $(\'.progress\').css({"display":"block"});
                                });
                                $(\'#preventModal form .btn-floating\').click(function(e){
                                    e.preventDefault();
                                    var data = $(\'#preventModal form div input\').val()
                                    if(data[data.length - 1] != "."){
                                        data = data + ".";
                                       $(\'#preventModal form div input\').val(data); 
                                    }
                                    $(\'#preventModal form\').submit();
                                });
                                $(\'input[name="temp"]\').change(function(){
                                    if((($(this).val() * 1.8) + 32) > 100.4){
                                        $(\'input[name="fever"]\').val("yes");
                                         Materialize.toast(
                                             \'You have a fever, \' 
                                             + $(\'input[name="name"]\').val().split(" ")[0], 
                                             8000);
                                    }
                                });

                            });
                        </script> 
                        <style>

                            .home-container{
                                padding: 5mm 2mm !important;
                                margin-top: 4mm !important;
                                border-radius: 3mm !important;
                            }

                            .about-body{
                                border-radius: 2mm;
                                margin-top: 5mm !important;
                                padding: 0mm 4mm 4mm 4mm !important;
                            }

                            .progress{
                                margin: 0 !important;
                                position: fixed !important;
                                top: 0 !important;
                            }

                            .modal{
                                width: 350px;
                            }

                            .modal h6{
                                font-size: 23px !important;
                            }

                            .not_here{
                                display: none;
                            }

                            @media screen and (min-width: 990px){
                                .brand-logo{
                                    margin-left: 4mm !important;
                                }
                            }
                        </style>                  
                    </head>'.

nav :-
        format(' <header style="position: relative; top: 0 !important;"> <nav class="green ligten-2"> <div class="nav-wrapper"> <a href="/" class="brand-logo">Swine Flu ES</a> <a href="#" data-activates="mobile-demo" class="button-collapse"><i class="material-icons">menu</i></a> <ul id="nav-mobile" class="right hide-on-med-and-down"> <li><a href="/">Home</a></li> <li><a href="/check">Checkup</a></li> <li><a href="/stat">Status Report</a></li> <li><a href="/about">About</a></li> </ul> <ul class="side-nav" id="mobile-demo"> <li><a href="/">Home</a></li> <li><a href="/check">Checkup</a></li> <li><a href="/stat">Status Report</a></li> <li><a href="/about">About</a></li> </ul> </div> </nav> </header><div class="progress"> <div class="indeterminate green"></div> </div> ',[]).

index(Request) :-
        format('Content-type: text/html~n~n', []),
        head(X),
        format('<html>
                    ~s
                    <body>',[X]),
            nav,
            format('    <div class="container">
                            <div class="container light-green lighten-5 home-container">
                                <h3 class="center"> SWINE FLU EXPERT SYSTEM </h3>
                                <p class="center">
                                    Welcome to the Swine Flu Expert System Web Application.<br>
                                    Where you can get quick and efficient swine flu check ups and information. 
                                </p><br>
                                <p>
                                    Think you might have the swine? Want to be sure? <a class="green-text wave-effect waves-light" href="/check"> Get A Checkup</a><br>
                                    Have flu prevention methods that you didn\'t find in the app? <a class="red-text wave-effect waves-light" href="/check#preventModal"> Update Preventions</a><br>
                                    Want to suggest other drugs for a specific symptom? <a class="yellow-text wave-effect waves-light" href="/check#drugModal"> Update Drugs</a><br>
                                </p>
                            </div>
                        </div>
                    </body>
                </htm>', []).

check(Request) :-
        format('Content-type: text/html~n~n', []),
        head(X),
        format('<html>
                    ~s
                    <body>',[X]),
            nav,
            format('
                    <script>
                        $(document).ready(function(){
                             if(window.location.hash == "#preventModal"){
                                 $(\'#preventModal\').modal(\'open\');
                             }else if(window.location.hash == "#drugModal"){
                                 $(\'#drugModal\').modal(\'open\');
                             }
                        });
                    </script>
            ',[]),
            format('    <div class="container">
                            <div class="fixed-action-btn">
                                <button class="btn-floating btn-large tooltipped" data-position="left" data-delay="80" data-tooltip="Update Options">
                                    <i class="material-icons">menu</i>
                                </button>
                                <ul>
                                    <li><a class="waves-effect waves-light btn-floating red tooltipped modal-trigger" href="#preventModal" data-position="left" data-delay="80" data-tooltip="Update Prevention Database"><i class="material-icons">flag</i></a></li>
                                    <li><a class="waves-effect waves-light btn-floating yellow darken-1 tooltipped modal-trigger" href="#drugModal" data-position="left" data-delay="80" data-tooltip="Update Drug Database"><i class="material-icons">local_pharmacy</i></a></li>
                                </ul>
                            </div>
                            <!-- Modal Structure -->
                            <div id="preventModal" class="modal">
                                <div class="modal-content">
                                    <h6>Update Prevention DataBase</h6>
                                    <br>
                                    <form action="/update/prevention" method="POST">
                                        <div class="input-field">
                                            <input type="text" name="prevention" />
                                            <label for="prevention">New Prevention</label>
                                        </div>
                                        <button class="btn-floating blue darken-4 right">
                                            <i class="material-icons">mode_edit</i>
                                        </button><br><br>
                                    </form>
                                </div>
                                <div class="modal-footer">
                                    <a href="#!" class="modal-action modal-close waves-effect waves-green btn-flat">Close</a>
                                </div>
                            </div>
                            <!-- Modal Structure -->
                            <div id="drugModal" class="modal">
                                <div class="modal-content">
                                    <h5>Update Drug DataBase</h5>
                                    <br>
                                    <form action="/update/drug" method="POST">
                                        <div class="input-field">
                                            <input type="text" name="drug" />
                                            <label for="drug">New Drug</label>
                                        </div>
                                        <div class="input-field">
                                            <input type="text" name="ailment" />
                                            <label for="ailment">Ailment It Treats</label>
                                        </div>
                                        <button class="btn-floating blue darken-4 right">
                                            <i class="material-icons">mode_edit</i>
                                        </button><br><br>
                                    </form>
                                </div>
                                <div class="modal-footer">
                                    <a href="#!" class="modal-action modal-close waves-effect waves-green btn-flat">Close</a>
                                </div>
                            </div>
                            <form action="/check/stat" method="POST">
                                <div class="container">
                                    <h3 class="center"> Check Your SWINE FLU Status </h3>
                                    <div class="input-field">
                                        <input name="name" type="text">
                                        <label for="name">Full Name</label>                                 
                                    </div>
                                    <div class="input-field">
                                        <input name="age"  type="number">
                                        <label for="age">Age</label>                                    
                                    </div>

                                    <br> <label> What\'s your gender? </label>
                                    <p>
                                        <input name="gender" type="radio" id="male" value="male"/>
                                        <label for="male">Male</label>
                                    </p>
                                    <p>
                                        <input name="gender" type="radio" id="female" value="female"/>
                                        <label for="female">Female</label>
                                    </p>
                                    <br>
                                    <div class="input-field">
                                        <input name="temp"  type="number">
                                        <label for="temp">What\'s your temperature (c &deg;)? </label>                                    
                                    </div>
                                    
                                    <h4 class="center"> Symptoms </h4>
                                    
                                    <br> <label> Do you have a cough? </label>
                                    <p>
                                        <input name="cough" type="radio" value="yes" id="cough_yes" />
                                        <label for="cough_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="cough" type="radio" value="no" id="cough_no" />
                                        <label for="cough_no"value="no">No</label>
                                    </p>
                                    <br> <label> Are your eyes watery and red? </label>
                                    <p>
                                        <input name="eyes" type="radio" value="yes" id="eyes_yes" />
                                        <label for="eyes_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="eyes" type="radio" value="no" id="eyes_no" />
                                        <label for="eyes_no"value="no">No</label>
                                    </p>
                                     <!--<br> <label> Do you think you have a fever? </label>
                                    <p>
                                        <input name="fever" type="radio" value="yes" id="fever_yes" />
                                        <label for="fever_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="fever" type="radio" value="no" id="fever_no" />
                                        <label for="fever_no"value="no">No</label>
                                    </p>-->
                                    <input hidden type="text" name="fever" value="no"/>
                                     <br> <label> Do you have an headache? </label>
                                    <p>
                                        <input name="headache" type="radio" value="yes" id="headache_yes" />
                                        <label for="headache_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="headache" type="radio" value="no" id="headache_no" />
                                        <label for="headache_no"value="no">No</label>
                                    </p>
                                     <br> <label> Is you throat sore?  </label>
                                    <p>
                                        <input name="throat" type="radio" value="yes" id="throat_yes" />
                                        <label for="throat_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="throat" type="radio" value="no" id="throat_no" />
                                        <label for="throat_no"value="no">No</label>
                                    </p>
                                     <br> <label> Have you been feeling more tired than usual? </label>
                                    <p>
                                        <input name="fatigue" type="radio" value="yes" id="fatigue_yes" />
                                        <label for="fatigue_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="fatigue" type="radio" value="no" id="fatigue_no" />
                                        <label for="fatigue_no"value="no">No</label>
                                    </p>
                                     <br> <label> Do you have nassale congestion or a runny nose? </label>
                                    <p>
                                        <input name="nose" type="radio" value="yes" id="nose_yes" />
                                        <label for="nose_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="nose" type="radio" value="no" id="nose_no" />
                                        <label for="nose_no"value="no">No</label>
                                    </p>
                                     <br> <label> Do you experience body aches? </label>
                                    <p>
                                        <input name="ache" type="radio" value="yes" id="ache_yes" />
                                        <label for="ache_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="ache" type="radio" value="no" id="ache_no" />
                                        <label for="ache_no"value="no">No</label>
                                    </p>
                                     <br> <label> Do you experience nausea? </label>
                                    <p>
                                        <input name="nausea" type="radio" value="yes" id="nausea_yes" />
                                        <label for="nausea_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="nausea" type="radio" value="no" id="nausea_no" />
                                        <label for="nausea_no"value="no">No</label>
                                    </p>
                                     <br> <label> Do you experience vomiting? </label>
                                    <p>
                                        <input name="vomitting" type="radio" value="yes" id="vomitting_yes" />
                                        <label for="vomitting_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="vomitting" type="radio" value="no" id="vomitting_no" />
                                        <label for="vomitting_no"value="no">No</label>
                                    </p>
                                     <br> <label> Are you sexually active? </label>
                                    <p>
                                        <input name="sActive" type="radio" value="yes" id="sActive_yes" />
                                        <label for="sActive_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="sActive" type="radio" value="no" id="sActive_no" />
                                        <label for="sActive_no"value="no">No</label>
                                    </p>
                                     <br> <label> Do you have diarrhea? </label>
                                    <p>
                                        <input name="diarrhea" type="radio" value="yes" id="diarrhea_yes" />
                                        <label for="diarrhea_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="diarrhea" type="radio" value="no" id="diarrhea_no" />
                                        <label for="diarrhea_no"value="no">No</label>
                                    </p>
                                    <br>
                                    <button id="doNtn" class="btn blue accent-3 right waves-effect waves-light"> Check </button>
                                </div>
                            </form><br><br><br><br>
                        </div>
                    </body>
                </htm>', []).

about(Request) :-
        head(X),
        format('Content-type: text/html~n~n', []),
        format('<html>
                    ~s

                    <body>',[X]),
        nav,
        format('
                        <div class="container">
                            <div class="container about-body grey lighten-4">
                                <h1> About SWine Flu </h1>
                                <p>
                                    Swine flu is a respiratory disease caused by influenza viruses that infect 
                                    the respiratory tract of pigs and result in a barking cough, decreased appetite, 
                                    nasal secretions, and listless behavior; the virus can be transmitted to humans. 
                                    Swine flu viruses may mutate (change) so that they are easily transmissible among 
                                    humans. The April 2009 swine flu outbreak (pandemic) was due to infection with the 
                                    H1N1 virus and was first observed in Mexico. Symptoms of swine flu in humans are 
                                    similar to most influenza infections: fever (100 F or greater), cough, nasal secretions, 
                                    fatigue, and headache.
                                </p>
                            </div>
                        </div>
                    </body>
                </htm>', []).

checkStat(Request) :-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),
        %length(Data, X),
        atom_number(Data.age, Age),
        (Data.gender == female, Age > 10, Age < 45 -> pregCheck(Request, Data); PT is 0, symptom_tally(RT, Data), statResults(Request, Data, RT, PT)).
        
get_preventions(X, li(class('collection-item'),X)) :-
                    prevention(Who).



prevent_para --> {
    findall(X,prevention(X),Prevents),
    maplist(get_preventions, Prevents,Paras)
}, html([\html_post(preventions, Paras)]).

get_prevent_para -->
                html([li(\html_receive(Preventions))]).



statResults(Request, Data, RT, PT) :-
        head(X),
        format('Content-type: text/html~n~n', []),
        atom_number(Data.temp, Temp), 
        FTemp is (Temp * 1.8 + 32),
        (FTemp > 100.4 -> TR is 20; TR is 0),
        TotalRisk is RT + PT + TR,
        atom_codes(TotalRisk, TtlRisk),
        (TotalRisk < 51 -> Title = 'You\'re definately not infected.', Color = 'green';
        TotalRisk > 50, TotalRisk < 160 -> Title = 'Not infected but you\'re at risk.', 
        Color = 'orange', assert(risk_patients(Data.name));
        Title = 'You have been infected with the swine.', Color = 'red',assert(infected_patients(Data.name))),
        assert(patients(Data.name)),
        %length(prevention(Who), Y),
        %findall(X,prevention(X),Paras),
        %maplist(get_preventions, Paras,Prevents),
        %aggregate_all(count, prevention(Who), Y),
        format('<html>
                    ~s

                    <body>',[X]),
        nav,
        format('
                        <div class="container">
                                <h1 class="center" > Your Status </h1>
                                <hr style="width: 80%; margin: 1mm auto !important;"/>
                                <h4 class="center">Results</h4>
                                    <div id="card-alert" class="card-alert card ~s lighten-5" style="width: 60%; margin: 2mm auto !important;">
                                        <div class="alert-content card-content ~s-text">
                                            <p class="center"> Risk Tally: ~s</p>
                                        </div>            
                                    </div>
                                <br>
                                 <h5>~s</h5>',[
                                        Color,
                                        Color,
                                        TtlRisk,
                                        Title]),
        (TotalRisk < 51 -> format('
                                <ul class="collection" id="prevents">
                                    <li class="collection-header">
                                        <h4 class="center">Suggested Preventions</h4>
                                        <hr class=\'grey lighten-5\'>
                                    </li>
                                    <li class="collection-item">
                                        You are okay, you don\'t need to worry..
                                    </li>
                                </ul><br><br>',[]);
        format('
                                <ul class="collection" id="prevents">
                                    <li class="collection-header">
                                        <h4 class="center">Suggested Preventions</h4>
                                        <hr class=\'grey lighten-5\'>
                                    </li>',[]),
        phrase(
            gen_page,
            TokenizedHtml,
            []),
        print_html(TokenizedHtml),
        format('                </ul>
                                <br><br>',[])),
        getDrug(Data),
        format('
                                <a class="btn blue accent-2" href="/">
                                    Done
                                </a><br><br><br><br>
                        </div>
                    </body>
                </htm>', [
                ]).
        

gen_page -->
        html([
            span(\prevent_para),
            span(\html_receive(preventions))

        ]).

pregCheck(Request, Data) :-        
        format('Content-type: text/html~n~n', []),
        head(X),
        format('<html>
                    ~s

                    <body>',[X]),
        nav,
        format('
                        <div class="container">
                            <div class="container">
                               <h3 class="center"> Pregnancy Check </h3>
                               <p class="center">
                                    ~s, since you\'re a female then we\'ll need to ask you a few questions
                               </p>
                               <form action="/pregCheck/Stat" method="POST">
                                    <input type="hidden" value="~s" name="name"/>
                                    <input type="hidden" value="~s" name="age"/>
                                    <input type="hidden" value="~s" name="gender"/>
                                    <input type="hidden" value="~s" name="temp"/>
                                    <input type="hidden" value="~s" name="cough"/>
                                    <input type="hidden" value="~s" name="eyes"/>
                                    <input type="hidden" value="~s" name="fever"/>
                                    <input type="hidden" value="~s" name="headache"/>
                                    <input type="hidden" value="~s" name="throat"/>
                                    <input type="hidden" value="~s" name="fatigue"/>
                                    <input type="hidden" value="~s" name="nose"/>
                                    <input type="hidden" value="~s" name="ache"/>
                                    <input type="hidden" value="~s" name="nausea"/>
                                    <input type="hidden" value="~s" name="vomitting"/>
                                    <input type="hidden" value="~s" name="sActive"/>
                                    <input type="hidden" value="~s" name="diarrhea"/>                                    
                                    <br> <label> Have you been craving odd food ? out of your diet ? </label>
                                    <p>
                                        <input name="food" type="radio" id="food_yes" value="yes"/>
                                        <label for="food_yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="food" type="radio" id="food_no" value="no"/>
                                        <label for="food_no">no</label>
                                    </p>
                                    <br> <label> Have you been swinging moods ? </label>
                                    <p>
                                        <input name="mood" type="radio" id="mood_yes" value="yes"/>
                                        <label for="mood_yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="mood" type="radio" id="mood_no" value="no"/>
                                        <label for="mood_no">No</label>
                                    </p>
                                    <br> <label> Was your last period more than 3 weeks ago ? </label>
                                    <p>
                                        <input name="bleed" type="radio" id="bleed_yes" value="yes"/>
                                        <label for="bleed_yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="bleed" type="radio" id="bleed_no" value="no"/>
                                        <label for="bleed_no">No</label>
                                    </p>
                                    <br> <label> Are your breats all tender? </label>
                                    <p>
                                        <input name="breast" type="radio" id="breast_yes" value="yes"/>
                                        <label for="breast_yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="breast" type="radio" id="breast_no" value="no"/>
                                        <label for="breast_no">No</label>
                                    </p>
                                    <br> <label> Been having frequent headaches ?  </label>
                                    <p>
                                        <input name="fheadache" type="radio" id="fheadache_yes" value="yes"/>
                                        <label for="fheadache_yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="fheadache" type="radio" id="fheadache_no" value="no"/>
                                        <label for="fheadache_no">No</label>
                                    </p>

                                    <button class="btn blue accent-2">Check</button>
                               </form>
                            </div>
                        </div>
                    </body>
                </htm>', [
                        Data.name, 
                        Data.name,
                        Data.age,
                        Data.gender,
                        Data.temp,
                        Data.cough,
                        Data.eyes,
                        Data.fever,
                        Data.headache,
                        Data.throat,
                        Data.fatigue,
                        Data.nose,
                        Data.ache,
                        Data.nausea,
                        Data.vomitting,
                        Data.sActive,
                        Data.diarrhea
                    ]).
                    

pregStat(Request) :-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),
        symptom_tally(RT, Data),
        p_symptom_tally(PT, Data),
        statResults(Request, Data, RT, PT).

updatePrev(Request):-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),
        asserta(prevention(Data.prevention)),
        index(Request).

updateDrug(Request):-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),
        asserta(drugFor(Data.drug,Data.ailment)),
        index(Request).

statusReport(Request) :-
        format('Content-type: text/html~n~n', []),
        head(X),
        format('<html>
                    ~s

                    <body>',[X]),
        nav,
        total_infected_patients(T),
        total_patients(K),
        total_risk_patients(F),
        percentage_of_infected(G,H,O),
        format('        <div class="container">
                            <br><h4 class="center"> Status Report </h4>
                            <hr style="width: 80% !important"> 
                            <br>
                            <table class="underlined">
                                <thead>
                                    <tr>
                                        <th>Data</th>
                                        <th>Rate</th>
                                    </tr>
                                </thead>
                                <tbody>
                                    <tr>
                                        <td>Total Number of Patients</td>
                                        <td>~d</td>
                                    </tr>
                                    <tr>
                                        <td>Total Number of Infected Patients</td>
                                        <td>~d</td>
                                    </tr>
                                    <tr>
                                        <td>Total Number of At Risk Patients</td>
                                        <td>~d</td>
                                    </tr>
                                    <tr>
                                        <td>Percentage of Patients Infected</td>
                                        <td>~d%</td>
                                    </tr>
                                </tbody>
                            </table>
                            <br><br><br>
                        </div>
                    </body>
                </html>',[K,T,F,O]).