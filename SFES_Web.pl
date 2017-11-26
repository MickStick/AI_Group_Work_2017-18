% Author: Wembley Williams, Mikhail Shaw
% Date: 10/27/2017

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_path)).
:- dynamic patient_sym/1.
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
p_symptom(PL0, Data)       :- Data.food == yes -> PL0 is 5; PL0 is 0.
p_symptom(PL1, Data)    :- Data.mood == yes -> PL1 is 5; PL1 is 0.
p_symptom(PL2, Data)         :- Data.period == yes -> PL2 is 5; PL2 is 0.
p_symptom(PL3, Data) :- Data.breast == yes -> PL3 is 5; PL3 is 0.
p_symptom(PL4, Data):- Data.bleed == yes -> PL4 is 5; PL4 is 0.

%Database of prevention methods
prevention('Stay home if you are sick.').
prevention('Wash your hands thoroughly and frequently.').
prevention('Contain your coughs and sneezes.').
prevention('Avoid contact.').
prevention('Reduce exposure within your household.').

%Allows the patient to add suggestive prevention means
suggestion(ToPrevent) :- asserta(prevention(ToPrevent)), write('Prevention measure added!').

%Tallying the risk of pregnancy which increases the risk of swine flu
p_symptom_tally(PT, Data) :- p_symptom(PL0, Data),
                       p_symptom(PL1, Data),
                       p_symptom(PL2, Data),
                       p_symptom(PL3, Data),
                       p_symptom(PL4, Data),
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
                                 var newHtml = $(\'#prevents\').text().replace("Content-type: text/html; charset=UTF-8","");
                                 console.log(newHtml);
                                 const newDiv = newHtml.split(".");
                                 $(\'#prevents\').empty();
                                 for(var x = 0;  x < newDiv.length; x++){
                                     var para = $("<p>" + newDiv[x] + "</p>");
                                     para.addClass("center");
                                    $(\'#prevents\').append(para);
                                 }
                                 $(\'.progress\').css({"display":"none"});
                                /*document.getElementById("doNtn").addEventListener("click", function(e){
                                    e.preventDefault();
                                });
                                $(\'#doNtn\').click(function(e){
                                    e.preventDefault();
                                    alert("This does nuffink!!");
                                });*/
                                $(\'input[name="gender"]\').change(function(){
                                    if($(this).val() == "male"){
                                        $(\'input[name="period"]\').attr("disabled","disabled");
                                    }else{
                                        $(\'input[name="period"]\').removeAttr("disabled");
                                    }
                                });
                                $(".button-collapse").sideNav();
                                $(window).on("beforeload", function(){
                                    $(\'.progress\').css({"display":"block"});
                                });

                                

                            });
                        </script> 
                        <style>
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
                        </style>                  
                    </head>'.

nav :-
        format(' <header style="position: relative; top: 0 !important;"> <nav class="green ligten-2"> <div class="nav-wrapper"> <a href="/" class="brand-logo">PL Server</a> <a href="#" data-activates="mobile-demo" class="button-collapse"><i class="material-icons">menu</i></a> <ul id="nav-mobile" class="right hide-on-med-and-down"> <li><a href="/">Home</a></li> <li><a href="/check">Check Up</a></li> <li><a href="/about">About</a></li> </ul> <ul class="side-nav" id="mobile-demo"> <li><a href="/">Home</a></li> <li><a href="/check">Check Up</a></li> <li><a href="/about">About</a></li> </ul> </div> </nav> </header><div class="progress"> <div class="indeterminate green"></div> </div> ',[]).

index(Request) :-
        format('Content-type: text/html~n~n', []),
        head(X),
        format('<html>
                    ~s
                    <body>',[X]),
            nav,
            format('    <div class="container">
                            <div class="container">
                                <h3 class="center"> SWINE FLU EXPERT SYSTEM </h3>
                                <p class="center">
                                    Welcome to the Swine Flu Expert System Web Application.<br>

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
            format('    <div class="container">
                            <form action="/check/stat" method="POST">
                                <div class="container">
                                    <h3 class="center"> Check Your SWINE FLU Status </h3>
                                    <div class="input-field">
                                        <input name="name" type="text">
                                        <label for="name">Name</label>                                 
                                    </div>
                                    <div class="input-field">
                                        <input name="age"  type="number">
                                        <label for="age">Age</label>                                    
                                    </div>

                                    <br> <label> Do you have a penis or a vagina? </label>
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
                                    
                                    <br> <label> Is you coughing, my guy? </label>
                                    <p>
                                        <input name="cough" type="radio" value="yes" id="cough_yes" />
                                        <label for="cough_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="cough" type="radio" value="no" id="cough_no" />
                                        <label for="cough_no"value="no">No</label>
                                    </p>
                                    <br> <label> Eyes all watery and red? </label>
                                    <p>
                                        <input name="eyes" type="radio" value="yes" id="eyes_yes" />
                                        <label for="eyes_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="eyes" type="radio" value="no" id="eyes_no" />
                                        <label for="eyes_no"value="no">No</label>
                                    </p>
                                     <br> <label> Got a fever, fam? </label>
                                    <p>
                                        <input name="fever" type="radio" value="yes" id="fever_yes" />
                                        <label for="fever_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="fever" type="radio" value="no" id="fever_no" />
                                        <label for="fever_no"value="no">No</label>
                                    </p>
                                     <br> <label> Killer headache? </label>
                                    <p>
                                        <input name="headache" type="radio" value="yes" id="headache_yes" />
                                        <label for="headache_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="headache" type="radio" value="no" id="headache_no" />
                                        <label for="headache_no"value="no">No</label>
                                    </p>
                                     <br> <label> Throat all sore and stuff?  </label>
                                    <p>
                                        <input name="throat" type="radio" value="yes" id="throat_yes" />
                                        <label for="throat_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="throat" type="radio" value="no" id="throat_no" />
                                        <label for="throat_no"value="no">No</label>
                                    </p>
                                     <br> <label> Been feeling really tired lately? </label>
                                    <p>
                                        <input name="fatigue" type="radio" value="yes" id="fatigue_yes" />
                                        <label for="fatigue_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="fatigue" type="radio" value="no" id="fatigue_no" />
                                        <label for="fatigue_no"value="no">No</label>
                                    </p>
                                     <br> <label> Is your nose running and stuffy? </label>
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
                                     <br> <label> Your period more than a month late? </label>
                                    <p>
                                        <input name="period" type="radio" value="yes" id="period_yes" />
                                        <label for="period_yes"value="yes">Yes</label>
                                    </p>
                                    <p>
                                        <input name="period" type="radio" value="no" id="period_no" />
                                        <label for="period_no"value="no">No</label>
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
                                <h1> About This Shit </h1>
                                <p>
                                    I don\'t really have much to say here, cuz uk, it\'s just a quick test.</br>
                                    But, anyways, this is me serving up some html with prolog. Not really serving up thou,
                                    but more like html-writing the page over to the client. Hmmmm, I wonder how people use
                                    prolog servers in real life. Like, where would you host it? Do they even still support
                                    prolog browsers? Are they even secure? Hmmmm, seeing how data is, it\'s prolly more 
                                    secure than SQL, but I don\'t really know what I\'m talking about.
                                </p>
                            </div>
                            <div class="fixed-action-btn">
                                <button class="btn-floating btn-large">
                                    <i class="material-icons">mode_edit</i>
                                </button>
                                <ul>
                                    <li><a class="btn-floating red"><i class="material-icons">insert_chart</i></a></li>
                                    <li><a class="btn-floating yellow darken-1"><i class="material-icons">format_quote</i></a></li>
                                    <li><a class="btn-floating green"><i class="material-icons">publish</i></a></li>
                                    <li><a class="btn-floating blue"><i class="material-icons">attach_file</i></a></li>
                                </ul>
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
        
get_preventions(X, p(class(center),X)) :-
                    prevention(Who).

prevent_para --> {
    findall(X,prevention(X),Prevents),
    maplist(get_preventions, Prevents,Paras)
}, html([\html_post(preventions, Paras)]).

get_prevent_para -->
                html([p(\html_receive(Preventions))]).

statResults(Request, Data, RT, PT) :-
        head(X),
        format('Content-type: text/html~n~n', []),
        atom_number(Data.temp, Temp), 
        FTemp is (Temp * 1.8 + 32),
        (FTemp > 100.4 -> TR is 20; TR is 0),
        TotalRisk is RT + PT + TR,
        atom_codes(TotalRisk, TtlRisk),
        (TotalRisk < 110 -> Title = 'Here are some suggestions for dodging the flu ',
        prevention(Who);
        Title = 'You have been infected with the swine', prevention(Who)),
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
                                    <div id="card-alert" class="card-alert card orange lighten-5" style="width: 60%; margin: 2mm auto !important;">
                                        <div class="alert-content card-content orange-text">
                                            <p class="center"> Risk Tally: ~s</p>
                                        </div>            
                                    </div>
                                <br>
                                <h5 class="center">~s</h5> <div id="prevents">',[
                                        TtlRisk,
                                        Title]),
        reply_html_page(
            title('SFES'),
            [\gen_page]
        ),
        format('                </div>
                                <p class="center">prevention Count: 0</p>
                                <br><br>
                                <a class="btn blue accent-2" href="/">
                                    Done
                                </a><br><br><br>
                        </div>
                    </body>
                </htm>', [
                ]).
        

gen_page -->
        html([
            p(\prevent_para),
            p(\html_receive(preventions))

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
                                    <input type="hidden" value="~s" name="period"/>
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
                                    <br> <label> Bleeding form the vaginal area? </label>
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
                        Data.period,
                        Data.nausea,
                        Data.vomitting,
                        Data.sActive,
                        Data.diarrhea
                    ]).
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

pregStat(Request) :-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),
        symptom_tally(RT, Data),
        p_symptom_tally(PT, Data),
        statResults(Request, Data, RT, PT).
        /*head(X),
        format('Content-type: text/html~n~n', []),
        format('<html>
                    ~s

                    <body>',[X]),
        nav,
        format('
                        <div class="container">
                            <div class="container about-body grey lighten-4">
                                <h1> About This Shit </h1>
                                <p>
                                    I don\'t really have much to say here, cuz uk, it\'s just a quick test.</br>
                                    But, anyways, this is me serving up some html with prolog. Not really serving up thou,
                                    but more like html-writing the page over to the client. Hmmmm, I wonder how people use
                                    prolog servers in real life. Like, where would you host it? Do they even still support
                                    prolog browsers? Are they even secure? Hmmmm, seeing how data is, it\'s prolly more 
                                    secure than SQL, but I don\'t really know what I\'m talking about.
                                </p>
                            </div>
                            <div class="fixed-action-btn">
                                <button class="btn-floating btn-large">
                                    <i class="material-icons">mode_edit</i>
                                </button>
                                <ul>
                                    <li><a class="btn-floating red"><i class="material-icons">insert_chart</i></a></li>
                                    <li><a class="btn-floating yellow darken-1"><i class="material-icons">format_quote</i></a></li>
                                    <li><a class="btn-floating green"><i class="material-icons">publish</i></a></li>
                                    <li><a class="btn-floating blue"><i class="material-icons">attach_file</i></a></li>
                                </ul>
                            </div>
                        </div>
                    </body>
                </htm>', []).*/