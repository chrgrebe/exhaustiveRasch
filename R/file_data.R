#' Activities of Daily Living - dichotomous example data
#'
#' ADL date from N=591 nursing home residents from Germany
#'
#' @format Activities of Daily Living (V06-V39: 1= yes; 0= no):
#' \itemize{
#'   \item V06: Transfer from bed independently.
#'   \item V07: Stand up from a chair/wheelchair independently.
#'   \item V08: Walk independently.
#'   \item V09: Stand independently.
#'   \item V11: micro-change positions in bed independently.
#'   \item V12: completely change position in bed independently.
#'   \item V22: Wash at a washbasin independently (without back and feet).
#'   \item V23: Wash back and feet independently
#'   \item V24: Take a shower independently
#'   \item V25: dress and undress the upper body independently
#'   \item V26: put on shoes and stockings independently
#'   \item V27: take a meal independently
#'   \item V28: drink independently from a prepared cup
#'   \item V36: use the toilet independently
#'   \item V39: intimate hygiene independently
#'   \item age: age (1= <65; 2= 65-74; 3= 75-84; 4= 85-94; 5= >=95)
#'   \item sex: age (1= female; 2= male)}
#' @source Data from Grebe (2013)
#' @references Grebe C (2013). Pflegeaufwand und Personalbemessung in der
#'  stationären Langzeitpflege: Entwicklung eines empirischen
#'   Fallgruppensystems auf der Basis von Bewohnercharakteristika.
#'    3-Länder-Konferenz Pflege & Pflegewissenschaft, Konstanz.

"ADL"


#' InterprofessionalCollaboration - polytomous example data
#'
#' data for certainty of action
#' measured with the Health Professionals Competence Scales (HePCoS-G).
#'  Dimension: interprofessional collaboration.
#' sample: N=716 nurses, midwifes, occupational therapists, physiotherapists
#'  and speech therapists.
#'
#' @format V01-V19: "How confident do you feel in the following situation?"
#'  (0= not confident; 1= hardly confident; 2= rather confident;
#'   3=very confident)
#' \itemize{
#'   \item V01: I communicate with other professions as part of my job.
#'   \item V02: I use information about a client from other professions.
#'   \item V03: I work with other professions when I transfer a client to their
#'    responsibility.
#'   \item V04: I work with other professions when I take over a client into
#'    my responsibility.
#'   \item V05: I involve specialists from other professions in the care of
#'    a client.
#'   \item V06: I coordinate goals of a client with the goals of other
#'    professions.
#'   \item V07: I coordinate health-promoting interventions for a client
#'    with other professions.
#'   \item V08: I inform other professions about the current status of a client.
#'   \item V09: I contribute aspects of a client's care to interprofessional
#'    case conferences.
#'   \item V10: I contribute aspects of a client's care to interprofessional
#'    case conferences and justify them on the basis of current
#'     research findings.
#'   \item V11: I contribute ethical aspects to interprofessional
#'    case conferences
#'   \item V12: I coordinate processes within an interprofessional team.
#'   \item V13: I coordinate communication processes during a conflict in an
#'    interprofessional team.
#'   \item V14: I lead an interprofessional team.
#'   \item V15: I participate in the development of interprofessional
#'    care concepts.
#'   \item V16: I participate in the development of an interdisciplinary
#'    evidence-based guideline.
#'   \item V17: I contribute my profession's perspective to discussions with
#'    members of other professions.
#'   \item V18: I contribute ethical aspects of care to a discussion with
#'    members of other professions.
#'   \item V19: I discuss the use of a new technology with members of other
#'    professions.
#'   \item profession: profession (1= physiotherapist;
#'    2= occupational therapist; 3= speech therapist; 4= nurse; 5= midwife)
#'   \item age: age (1= <20; 2= 20-24; 3= 25-29; 4= 30-34; 5= 35-39; 6= 40-44;
#'    7= 45-49; 8= 50-54; 9= 55-59; 10= >=60)
#'   \item sex: sex (1= male; 2= female)
#'   \item job_experience: job experience in years (including education)}
#' @source Data from the construction sample of the Health Professionals
#'  Competence Scales (HePCoS)
#' @references Grebe C, Schürmann M, Latteck, ÄD (2021). Die HePCoS-Skalen
#'  zur Kompetenzerfassung in den Gesundheitsfachberufen. Berichte aus
#'   Forschung und Lehre (48). Bielefeld, Fachhochschule Bielefeld.
#'    DOI: http://dx.doi.org/10.13140/RG.2.2.13480.08967/1

"InterprofessionalCollaboration"

#' cognition - polytomous example data
#' data measured with the FACT-cog (subscale perceived cognitive functioning)
#' sample size: N=1009
#'
#' @format Items 1-20: FACT-cog Items. Questions starting with
#'  In the past 7 days...
#'  Answer categories: 0= never; 1= about once a week; 2= two to three times
#'  a week;  3=nearly every day; 4= several times a day
#' \itemize{
#'   \item A1_CogA1: I have had trouble forming thoughts
#'   \item A3_CogA3: My thinking has been slow
#'   \item C7_PC8: I have had trouble concentrating
#'   \item M9_PC10: I have had trouble finding my way to a familiar place
#'   \item M10_PC11: I have had trouble remembering where I put things, like my keys or my wallet
#'   \item M12_NQCOG68: I have had trouble remembering new information, like phone numbers or simple instructions
#'   \item V13_PC14: I have had trouble recalling the name of an object while talking to
#'   \item V15_PC16: I have had trouble finding the right word(s) to express
#'   \item V16_CogV16: I have used the wrong word when I referred to an object
#'   \item V17_CogV17b: I have had trouble saying what I mean in conversations with others
#'   \item F19_NQCOG69: I have walked into a room and forgot what I meant to get or do there
#'   \item F23_NQCOG77: I had to work really hard to pay attention or I would make a mistake.
#'   \item F24_PC26: I have forgotten names of people soon after being introduced
#'   \item F25_PC28: My reactions in everyday situations have been
#'   \item C31_PC36: I have had to work harder than usual to keep track of what I was
#'   \item C32_PC37: My thinking has been slower than usual
#'   \item C33a_PC38: I have had to work harder than usual to express myself clearly
#'   \item C33c_PC40: I have had to use written lists more often than usual so I would not forget things
#'   \item MT1_PC41:  I have had trouble keeping track of what I was doing when interrupted
#'   \item MT2_PC42: I have had trouble shifting back and forth between different activities that require thinking
#'   \item CogPCI: Subscale Score: Percieved Cognitive Impairments (CogPCI), Range 0-72
#'   \item SD1: What is your age?
#'   \item SD2: What is your gender? (1= male; 2= female)
#'   \item SD3: Are you of Hispanic, Latino, or Spanish origin? (0= no; 1= yes)
#'   \item SD4: In which country were you born? (1= United States of America; 2= Mexico; 3= Canada; 4= United Kingdom; 5= other)
#'   \item SD6: What is the HIGHEST level of school or degree you have completed? (1= Less than High School Degree; 2= High School Diploma or General Education Diploma (GED); 3= Some college courses/Technical degree/ Vocational program/Associate degree; 4= College degree/advanced degree (Bachelors, Masters, Doctorate))
#'   \item SD7: In the past 30 days, have you used or taken medication for which a prescription is needed?  Include only those products prescribed by a health professional such as a doctor or dentist.  (0= no; 1= yes)
#'   \item SD8: How many different times did you stay in any hospital overnight or longer during the past 12 months?
#'   \item SD9: During the past 30 days, for about how many days did poor physical or mental health keep you from doing your usual activities such as self-care, work, or recreation?}
#' @source Data for perceived cognitive functioning from the PROsetta Stone Wave 2
#' dataset.
#' @references Cella D (2017). PROsetta Stone Wave 2. Harvard Dataverse.
#' DOI: 10.7910/DVN/WZVYZI.

"cognition"

