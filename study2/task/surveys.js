//** set up surveys presented in 'index.html' **//

// define Ipsos preamble
var preambleIpsos = "<p>Please think about your most recent interaction at a shop, store, bank, or anywhere else where you were a face to face customer.</p>";

// define Ipsos items (for Qs 2,3,5: randomize order of all responses except None of the above/Don't know)
var itemIpsos1 = [{
  prompt: "Were you or the staff member you interacted with wearing a face mask?",
  name: 'Ipsos1',
  options: ["We both were", "Only the staff member was", "Only I was", "Neither of us were", "Don\'t know/can\'t remember"],
  required: true
}];

var itemIpsos4 = [{
  prompt: "Thinking about what it was that you set out to do, which of the following best applies?",
  name: 'Ipsos4',
  options: ["Mask wearing prevented me from achieving what I wanted to do", "Mask wearing made it harder for me to achieve what I wanted to do", "Mask wearing had no impact on me achieving what I wanted to do", "Mask wearing made it easier for me to achieve what I wanted to do", "Don\'t know"],
  required: true
}];

var allOptionsIpsos2 =  ["The staff member could not hear me", "The staff member could not understand me", "I could not attract the attention of a staff member", "The face mask made me physically uncomfortable so I cut my trip short", "I felt like the staff were laughing at me because I was wearing a mask", "I enjoyed my trip less", "I received a poorer standard of customer service"];
var shuffleOptionsIpsos2 = jsPsych.randomization.shuffle(allOptionsIpsos2);
var shuffleOptionsIpsos2 = shuffleOptionsIpsos2.concat(["None of the above", "Don\'t know"]);

var itemIpsos2 = [{
  prompt: "Which, if any, of the following happened as a result of you wearing a face mask?", 
  name: 'Ipsos2',
  options: shuffleOptionsIpsos2,
  required: true
}];

var allOptionsIpsos3 = ["I could not hear the staff member", "I could not understand the staff member", "I could not tell if the staff member was being serious when they made recommendations", "The staff member appeared physically uncomfortable in their mask", "The staff member appeared self-conscious in their mask", "I enjoyed my trip less", "I received a poorer standard of customer service"]; 
var shuffleOptionsIpsos3 = jsPsych.randomization.shuffle(allOptionsIpsos3);
var shuffleOptionsIpsos3 = shuffleOptionsIpsos3.concat(["None of the above", "Don\'t know"]);

var itemIpsos3 = [{
  prompt: "Which, if any, of the following happened as a result of the staff member wearing a face mask?", 
  name: 'Ipsos3',
  options: shuffleOptionsIpsos3,
  required: true
}];

var allOptionsIpsos5 = ["More signs or posters on site to tell me what to do", "Staff speaking more slowly", "Staff speaking more loudly", "No/lower music on site so it was easier to hear what was going on", "Staff being more patient/taking more time with me", "Less staff interaction because I didn\'t want help", "Staff having visual aids (e.g. tablets or signs) to tell me what to do", "Staff wearing friendly-looking masks"];
var shuffleOptionsIpsos5 = jsPsych.randomization.shuffle(allOptionsIpsos5);
var shuffleOptionsIpsos5 = shuffleOptionsIpsos5.concat(["Other", "Don\'t know"]);

var itemIpsos5 = [{
  prompt: "Again thinking about what it was that you set out to do, which of the following would have made it easier to achieve what you wanted to do?",
  name: 'Ipsos5',
  options: shuffleOptionsIpsos5,
  required: true
}];


// Ipsos
var ipsos1 = {
  type: 'survey-multi-choice',
  preamble: preambleIpsos,
  questions: itemIpsos1,
  post_trial_gap: 500,
  data: {whichWindow: 'questionnaire', whichQ: 'Ipsos'},
  on_finish: function(data){
    var dataIpsos1 = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    data.ipsos1 = dataIpsos1['Ipsos1'];
  }
};

var ipsos2 = {
  type: 'survey-multi-select',
  preamble: preambleIpsos,
  questions: itemIpsos2,
  post_trial_gap: 500,
  data: {whichWindow: 'questionnaire', whichQ: 'Ipsos'},
  on_finish: function(data){
    var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    var ipsos2Index = Array.apply(null, {length: qData['Ipsos2'].length}).map(Number.call, Number);
    // loop through listed responses
    ipsos2Index.forEach(function(i) {
      data['ipsos2_' + i] = qData['Ipsos2'][i];
    })
  }
};

var ipsos3 = {
  type: 'survey-multi-select',
  preamble: preambleIpsos,
  questions: itemIpsos3,
  post_trial_gap: 500,
  data: {whichWindow: 'questionnaire', whichQ: 'Ipsos'},
  on_finish: function(data){
    var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    var ipsos3Index = Array.apply(null, {length: qData['Ipsos3'].length}).map(Number.call, Number);
    // loop through listed responses
    ipsos3Index.forEach(function(i) {
      data['ipsos3_' + i] = qData['Ipsos3'][i];
    })
  }
};

var ipsos4 = {
  type: 'survey-multi-choice',
  preamble: preambleIpsos,
  questions: itemIpsos4,
  post_trial_gap: 500,
  data: {whichWindow: 'questionnaire', whichQ: 'Ipsos'},
  on_finish: function(data){
    var dataIpsos4 = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    data.ipsos4 = dataIpsos4['Ipsos4'];
  }
};

var ipsos5 = {
  type: 'survey-multi-select',
  preamble: preambleIpsos,
  questions: itemIpsos5,
  post_trial_gap: 500,
  data: {whichWindow: 'questionnaire', whichQ: 'Ipsos'},
  on_finish: function(data){
    var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    var ipsos5Index = Array.apply(null, {length: qData['Ipsos5'].length}).map(Number.call, Number);
    // loop through listed responses
    ipsos5Index.forEach(function(i) {
      data['ipsos5_' + i] = qData['Ipsos5'][i];
    })
  }
};

// specify answer to ipsos5 if 'other' selected
var ipsos5Other = {
  type: 'survey-html-form',
  preamble: '<p> Because you selected <i>other</i>, please specify here: </p>',
  html: '<p> Other <input name="ipsos5Other" type="text" /></p>',
  post_trial_gap: 500,
  data: {whichWindow: 'questionnaire', whichQ: 'Ipsos'},
  on_finish: function(data){
    var dataIpsos5Other = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    data.ipsos5Other = dataIpsos5Other['ipsos5Other'];
  }
};

var ifNodeIpsos5Other = {
  timeline: [ipsos5Other],
  conditional_function: function(){
    var dataIpsos5 = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    var responseIpsos5 = dataIpsos5['Ipsos5'].includes('Other');
    if(responseIpsos5 == true){
      return true;
    } else {
      return false;
    }
  }
};

var ifNodeIpsos1a = {
  timeline: [ipsos2, ipsos3, ipsos4, ipsos5, ifNodeIpsos5Other],
  conditional_function: function(){
    var dataIpsos1 = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    var responseIpsos1 = dataIpsos1['Ipsos1']=='We both were' || dataIpsos1['Ipsos1']=='Only I was';
    if(responseIpsos1 == true){
      return true;
    } else {
      return false;
    }
  }
};

var ifNodeIpsos1b = {
  timeline: [ipsos4, ipsos5, ifNodeIpsos5Other],
  conditional_function: function(){
    var dataIpsos1 = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    var responseIpsos1 = dataIpsos1['Ipsos1']=='Only the staff member was';
    if(responseIpsos1 == true){
      return true;
    } else {
      return false;
    }
  }
};

// define questionnaire scales
var scaleAQC = [
  "Definitely disagree",
  "Slightly disagree",
  "Slightly agree",
  "Definitely agree"
];

var scaleBEQS = [
  "(1) Strongly disagree",
  "(2)",
  "(3)",
  "(4) Neutral",
  "(5)",
  "(6)",
  "(7) Strongly agree"
];

var scaleBFI = [
  "(1) Disagree strongly",
  "(2) Disagree a little",
  "(3) Neither agree nor disagree",
  "(4) Agree a little",
  "(5) Agree strongly"
];

var scaleIRQ = [
  "(1) Strongly disagree",
  "(2) Disagree",
  "(3) Somewhat disagree",
  "(4) Neither agree nor disagree",
  "(5) Somewhat agree",
  "(6) Agree",
  "(7) Strongly agree"
];

var scaleINDCOL = [
  "(1) Strongly disagree",
  "(2) Disagree",
  "(3) Somewhat disagree",
  "(4) Neither agree nor disagree",
  "(5) Somewhat agree",
  "(6) Agree",
  "(7) Strongly agree"
];

var scalePolitical = [
  "(1) Extremely liberal",
  "(2) Liberal",
  "(3) Somewhat liberal",
  "(4) Neither liberal nor conservative",
  "(5) Somewhat conservative",
  "(6) Conservative",
  "(7) Extremely conservative"
];

// define questionnaire items
var itemsAQC = [
  {
    prompt: "Other people frequently tell me that what I've said is impolite, even though I think it is polite.",
    name: 'AQC1',
    labels: scaleAQC,
    required: true
  },
  {
    prompt: "I enjoy social chit-chat.",
    name: 'AQC2',
    labels: scaleAQC,
    required: true
  },
  {
    prompt: "When I talk, it isn't always easy for others to get a word in edgeways.",
    name: 'AQC3',
    labels: scaleAQC,
    required: true
  },
  {
    prompt: "I frequently find that I don't know how to keep a conversation going.",
    name: 'AQC4',
    labels: scaleAQC,
    required: true
  },
  {
    prompt: 'I find it easy to "read between the lines" when someone is talking to me.',
    name: 'AQC5',
    labels: scaleAQC,
    required: true
  },
  {
    prompt: "I know how to tell if someone listening to me is getting bored.",
    name: 'AQC6',
    labels: scaleAQC,
    required: true
  },
  {
    prompt: "When I talk on the phone, I'm not sure when it's my turn to speak.",
    name: 'AQC7',
    labels: scaleAQC,
    required: true
  },
  {
    prompt: "I am often the last to understand the point of a joke.",
    name: 'AQC8',
    labels: scaleAQC,
    required: true
  },
  {
    prompt: "I am good at social chit-chat",
    name: 'AQC9',
    labels: scaleAQC,
    required: true
  },
  {
    prompt: "People often tell me that I keep going on and on about the same thing.",
    name: 'AQC10',
    labels: scaleAQC,
    required: true
  }
];

var itemsBEQS = [
  {
    prompt: "I sometimes cry during sad movies.",
    name: 'BEQS1',
    labels: scaleBEQS,
    required: true
  },
  {
    prompt: "My body reacts very strongly to emotional situations.",
    name: 'BEQS2',
    labels: scaleBEQS,
    required: true
  },
  {
    prompt: "I have strong emotions",
    name: 'BEQS3',
    labels: scaleBEQS,
    required: true
  },
  {
    prompt: "I am sometimes unable to hide my feelings, even though I would like to.",
    name: 'BEQS4',
    labels: scaleBEQS,
    required: true
  },
  {
    prompt: "There have been times when I have not been able to stop crying even though I tried to stop.",
    name: 'BEQS5',
    labels: scaleBEQS,
    required: true
  },
  {
    prompt: "I experience my emotions very strongly.",
    name: 'BEQS6',
    labels: scaleBEQS,
    required: true
  }
];

var itemsBFI1 = [
  {
    prompt: "Is talkative.",
    name: 'BFI1',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Tends to find fault with others",
    name: 'BFI2',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Does a thorough job",
    name: 'BFI3',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is depressed, blue",
    name: 'BFI4',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is original, comes up with new ideas",
    name: 'BFI5',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is reserved",
    name: 'BFI6',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is helpful and unselfish with others",
    name: 'BFI7',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Can be somewhat careless",
    name: 'BFI8',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is relaxed, handles stress well",
    name: 'BFI9',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is curious about many things",
    name: 'BFI10',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is full of energy",
    name: 'BFI11',
    labels: scaleBFI,
    required: true
  }
];

var itemsBFI2 = [
  {
    prompt: "Starts quarrels with others",
    name: 'BFI12',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is a reliable worker",
    name: 'BFI13',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Can be tense",
    name: 'BFI14',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is ingenious, a deep thinker",
    name: 'BFI15',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Generates a lot of enthusiasm",
    name: 'BFI16',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Has a forgiving nature",
    name: 'BFI17',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Tends to be disorganised",
    name: 'BFI18',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Worries a lot",
    name: 'BFI19',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Has an active imagination",
    name: 'BFI20',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Tends to be quiet",
    name: 'BFI21',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is generally trusting",
    name: 'BFI22',
    labels: scaleBFI,
    required: true
  }
];

var itemsBFI3 = [
  {
    prompt: "Tends to be lazy",
    name: 'BFI23',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is emotionally stable, not easily upset",
    name: 'BFI24',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is inventive",
    name: 'BFI25',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Has an assertive personality",
    name: 'BFI26',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Can be cold and aloof",
    name: 'BFI27',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Perseveres until the task is finished",
    name: 'BFI28',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Can be moody",
    name: 'BFI29',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Values artistic, aesthetic experiences",
    name: 'BFI30',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is sometimes shy, inhibited",
    name: 'BFI31',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is considerate and kind to almost everyone",
    name: 'BFI32',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Does things efficiently",
    name: 'BFI33',
    labels: scaleBFI,
    required: true
  }
];

var itemsBFI4 = [
  {
    prompt: "Remains calm in tense situations",
    name: 'BFI34',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Prefers work that is routine",
    name: 'BFI35',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is outgoing, sociable",
    name: 'BFI36',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is sometimes rude to others",
    name: 'BFI37',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Makes plans and follows through with them",
    name: 'BFI38',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Gets nervous easily",
    name: 'BFI39',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Likes to reflect, play with ideas",
    name: 'BFI40',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Has few artistic interests",
    name: 'BFI41',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Likes to cooperate with others",
    name: 'BFI42',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is easily distracted",
    name: 'BFI43',
    labels: scaleBFI,
    required: true
  },
  {
    prompt: "Is sophisticated in art, music, or literature",
    name: 'BFI44',
    labels: scaleBFI,
    required: true
  }
];

var itemsIRQ1 = [
  {
    prompt: "When things are going well, I feel compelled to seek out other people.",
    name: 'IRQ1',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "It really helps me feel better during stressful situations when someone knows and cares about what I'm going through.",
    name: 'IRQ2',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "I find that even just being around other people can help me to feel better.",
    name: 'IRQ3',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "When something good happens, my first impulse is to tell someone about it.",
    name: 'IRQ4',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "When something bad happens, my first impulse is to seek out the company of others.",
    name: 'IRQ5',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "When I want to celebrate something good, I seek out certain people to tell them about it.",
    name: 'IRQ6',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "When things are going well, I just have to tell other people about it.",
    name: 'IRQ7',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "I appreciate having others' support through difficult times.",
    name: 'IRQ8',
    labels: scaleIRQ,
    required: true
  }
];

var itemsIRQ2 = [
  {
    prompt: "I really enjoy being around the people I know.",
    name: 'IRQ9',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "I manage my emotions by expressing them to others.",
    name: 'IRQ10',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "I really appreciate having other people to help me figure out my problems.",
    name: 'IRQ11',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "Sometimes I just need someone to understand where I'm coming from.",
    name: 'IRQ12',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "I just have to get help from someone when things are going wrong.",
    name: 'IRQ13',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "I'm happier when I'm with my friends than when I'm by myself",
    name: 'IRQ14',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "When I'm having trouble, I can't wait to tell someone about it.",
    name: 'IRQ15',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "Being with other people tends to put a smile on my face.",
    name: 'IRQ16',
    labels: scaleIRQ,
    required: true
  },
  {
    prompt: "Select somewhat disagree for this statement.",
    name: 'attentionCheck',
    labels: scaleIRQ,
    required: true
  }
];

var itemsINDCOL1 = [
  {
    prompt: "My happiness depends very much on the happiness of those around me.",
    name: 'INDCOL1',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: 'I would do what would please my family, even if I detested that activity.',
    name: 'INDCOL2',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "I usually sacrifice my self-interest for the benefit of my group.",
    name: 'INDCOL3',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "I enjoy working in situations involving competition with others.",
    name: 'INDCOL4',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "The well-being of my co-workers is important to me.",
    name: 'INDCOL5',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "I enjoy being unique and different from others in many ways.",
    name: 'INDCOL6',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "Children should feel honored if their parents receive a distinguished award.",
    name: 'INDCOL7',
    labels: scaleINDCOL,
    required: true
  }
];

var itemsINDCOL2 = [
  {
    prompt: "I often 'do my own thing'.",
    name: 'INDCOL8',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "Competition is the law of nature.",
    name: 'INDCOL9',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "If a co-worker gets a prize, I would feel proud.",
    name: 'INDCOL10',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "I am a unique individual.",
    name: 'INDCOL11',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "I would sacrifice an activity that I enjoy very much if my family did not approve of it.",
    name: 'INDCOL12',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "Without competition it is not possible to have a good society.",
    name: 'INDCOL13',
    labels: scaleINDCOL,
    required: true
  },
  {
    prompt: "I feel good when I cooperate with others.",
    name: 'INDCOL14',
    labels: scaleINDCOL,
    required: true
  }
];

var itemsDemographics = [
  // {
  //   prompt: "What is your last name?",
  //   name: 'LastName',
  //   required: true
  // }, 
  // {
  //   prompt: "What is your first name?",
  //   name: 'FirstName',
  //   required: true
  // },
  // {
  //   prompt: "Please enter your preferred email address",
  //   name: 'Email',
  //   required: true
  // },
  {
    prompt: "Please enter your age.",
    name: 'Age',
    required: true
  }
];

var itemGender = [{
  prompt: "Please indicate your gender.",
  name: 'Gender',
  options: ['Male', 'Female', 'Other'],
  // options: ['Male', 'Female', 'Other', 'Choose not to disclose'],
  required: true
}];

var itemEthnicity = [{
  prompt: "Please enter your race/ethnic affiliation. Check all that apply.",
  name: 'Ethnicity',
  options: ['Indigenous', 'East Asian', 'South Asian', 'Black or African', 'White or Caucasian', 'Hispanic or Latino', 'Middle Eastern', 'Other'],
  // options: ['Indigenous', 'East Asian', 'South Asian', 'Black or African', 'White or Caucasian', 'Hispanic or Latino', 'Middle Eastern', 'Other', 'Choose not to disclose'],
  required: true
}];

var itemPolitical = [{
  prompt: "Please describe your political views.",
  name: 'Ideology',
  labels: scalePolitical,
  required: true
}];

// var itemFutureStudies = [{
//   prompt: "Is it okay if we contact you about future studies?",
//   name: 'Future',
//   options: ['Yes', 'No'],
//   required: true
// }];

// combine questionnaire items
var itemsBFI = itemsBFI1.concat(itemsBFI2, itemsBFI3, itemsBFI4);
var itemsIRQ = itemsIRQ1.concat(itemsIRQ2);
var itemsINDCOL = itemsINDCOL1.concat(itemsINDCOL2);

// combine questionnaire sections
var sectionsAQC = [itemsAQC];
var sectionsBEQS = [itemsBEQS];
var sectionsBFI = [itemsBFI1, itemsBFI2, itemsBFI3, itemsBFI4];
var sectionsIRQ = [itemsIRQ1, itemsIRQ2];
var sectionsINDCOL = [itemsINDCOL1, itemsINDCOL2];

// define preamble text
var preambleAQC = "<p>Below are a list of statements. " +
  "Please read each statement very carefully and rate how strongly you agree or disagree with it by selecting your answer.</p>";

var preambleBEQS = "<p>For each statement below, please indicate your agreement or disagreement. " +
  "Do so by choosing the appropriate number from 1 (strongly disagree) to 7 (strongly agree).</p>";

var preambleBFI = "<p>Here are a number of characteristics that may or may not apply to you. " +
  "For example, do you agree that you are someone who <i>likes to spend time with others?</i> " + 
  "Please select a number next to each statement to indicate the extent to which <b>you agree or disagree with that statement.</b></p>" +
  "<p>I am someone who...</p>";

var preambleIRQ = "<p>Please indicate how much you agree or disagree with each of the following statements.</p>";

var preambleINDCOL = "<p>Please select the option that best indicates how you feel about each statement.</p>";


// set up questionnaires (log responses and attention check within IRQ)
function createAQC() {
  array = []
  // loop through questionnaire sections
  var sectionIndex = Array.apply(null, {length: sectionsAQC.length}).map(Number.call, Number);
  sectionIndex.forEach(function(i) {
    var object = {}
    object['type'] = 'survey-likert'
    object['preamble'] = preambleAQC
    object['questions'] = sectionsAQC[i]
    object['post_trial_gap'] = 500
    object['data'] = {whichWindow: 'questionnaire', whichQ: 'AQC'}
    object['on_finish'] = function(data){
      var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses)
      // loop through items within each questionnaire section
      var itemIndex = Array.apply(null, {length: itemsAQC.length}).map(Number.call, Number);
      itemIndex.forEach(function(j) {
        j++
        data['AQC' + j] = qData['AQC' + j] 
      })
    }
  array.push(object)
  })
  return array
}

function createBEQS() {
  array = []
  // loop through questionnaire sections
  var sectionIndex = Array.apply(null, {length: sectionsBEQS.length}).map(Number.call, Number);
  sectionIndex.forEach(function(i) {
    var object = {}
    object['type'] = 'survey-likert'
    object['preamble'] = preambleBEQS
    object['questions'] = sectionsBEQS[i]
    object['post_trial_gap'] = 500
    object['data'] = {whichWindow: 'questionnaire', whichQ: 'BEQS'}
    object['on_finish'] = function(data){
      var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses)
      // loop through items within each questionnaire section
      var itemIndex = Array.apply(null, {length: itemsBEQS.length}).map(Number.call, Number);
      itemIndex.forEach(function(j) {
        j++
        data['BEQS' + j] = qData['BEQS' + j] 
      })
    }
  array.push(object)
  })
  return array
}

function createBFI() {
  array = []
  // loop through questionnaire sections
  var sectionIndex = Array.apply(null, {length: sectionsBFI.length}).map(Number.call, Number);
  sectionIndex.forEach(function(i) {
    var object = {}
    object['type'] = 'survey-likert'
    object['preamble'] = preambleBFI
    object['questions'] = sectionsBFI[i]
    object['post_trial_gap'] = 500
    object['data'] = {whichWindow: 'questionnaire', whichQ: 'BFI'}
    object['on_finish'] = function(data){
      var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses)
      // loop through items within each questionnaire section
      var itemIndex = Array.apply(null, {length: itemsBFI.length}).map(Number.call, Number);
      itemIndex.forEach(function(j) {
        j++
        data['BFI' + j] = qData['BFI' + j] 
      })
    }
  array.push(object)
  })
  return array
}

function createIRQ() {
  array = []
  // loop through questionnaire sections
  var sectionIndex = Array.apply(null, {length: sectionsIRQ.length}).map(Number.call, Number);
  sectionIndex.forEach(function(i) {
    var object = {}
    object['type'] = 'survey-likert'
    object['preamble'] = preambleIRQ
    object['questions'] = sectionsIRQ[i]
    object['post_trial_gap'] = 500
    object['data'] = {whichWindow: 'questionnaire', whichQ: 'IRQ'}
    object['randomize_question_order'] = true
    object['on_finish'] = function(data){
      var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses)
      // loop through items within each questionnaire section
      var itemIndex = Array.apply(null, {length: itemsIRQ.length}).map(Number.call, Number);
      itemIndex.forEach(function(j) {
        j++
        if (j < itemsIRQ.length) {
          data['IRQ' + j] = qData['IRQ' + j]
        } else if (j == itemsIRQ.length) {
          data.attentionCheck = qData['attentionCheck']
        }
      })
    }
  array.push(object)
  })
  return array
}

function createINDCOL() {
  array = []
  // loop through questionnaire sections
  var sectionIndex = Array.apply(null, {length: sectionsINDCOL.length}).map(Number.call, Number);
  sectionIndex.forEach(function(i) {
    var object = {}
    object['type'] = 'survey-likert'
    object['preamble'] = preambleINDCOL
    object['questions'] = sectionsINDCOL[i]
    object['post_trial_gap'] = 500
    object['data'] = {whichWindow: 'questionnaire', whichQ: 'INDCOL'}
    object['on_finish'] = function(data){
      var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses)
      // loop through items within each questionnaire section
      var itemIndex = Array.apply(null, {length: itemsINDCOL.length}).map(Number.call, Number);
      itemIndex.forEach(function(j) {
        j++
        data['INDCOL' + j] = qData['INDCOL' + j] 
      })
    }
  array.push(object)
  })
  return array
}

// create questionnaires
var AQC = createAQC();
var BEQS = createBEQS();
var BFI = createBFI();
var IRQ = createIRQ();
var INDCOL = createINDCOL();

// demographics
var demographics = {
  type: 'survey-text',
  questions: itemsDemographics,
  post_trial_gap: 500,
  data: {whichWindow: 'demographics'},
  on_finish: function(data){
    var dataDemographics = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    data.age = parseInt(dataDemographics['Age']);
  }
};

// gender
var gender = {
  type: 'survey-multi-choice',
  questions: itemGender,
  post_trial_gap: 500,
  data: {whichWindow: 'demographics'},
  on_finish: function(data){
    var dataGender = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    data.gender = dataGender['Gender'];
  }
};

// specify gender if 'other' selected
var genderOther = {
  type: 'survey-html-form',
  preamble: '<p> Because you selected <i>other</i>, please specify here: </p>',
  html: '<p> Other <input name="genderOther" type="text" /></p>',
  post_trial_gap: 500,
  data: {whichWindow: 'demographics'},
  on_finish: function(data){
    var dataGenderOther = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    data.genderOther = dataGenderOther['genderOther'];
  }
};

var ifNodeGenderOther = {
  timeline: [genderOther],
  conditional_function: function(){
    var dataGender = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    var responseGender = dataGender['Gender']=='Other';
    if(responseGender == true){
      return true;
    } else {
      return false;
    }
  }
};

// ethnicity
var ethnicity = {
  type: 'survey-multi-select',
  questions: itemEthnicity,
  post_trial_gap: 500,
  data: {whichWindow: 'demographics'},
  on_finish: function(data){
    var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    var ethnicityIndex = Array.apply(null, {length: qData['Ethnicity'].length}).map(Number.call, Number);
    // loop through listed ethnicities
    ethnicityIndex.forEach(function(i) {
      data['ethnicity' + i] = qData['Ethnicity'][i];
    })
  }
};

// specify ethnicity if 'other' selected
var ethnicityOther = {
  type: 'survey-html-form',
  preamble: '<p> Because you selected <i>other</i>, please specify here: </p>',
  html: '<p> Other <input name="ethnicityOther" type="text" /></p>',
  post_trial_gap: 500,
  data: {whichWindow: 'demographics'},
  on_finish: function(data){
    var dataEthnicityOther = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    data.ethnicityOther = dataEthnicityOther['ethnicityOther'];
  }
};

var ifNodeEthnicityOther = {
  timeline: [ethnicityOther],
  conditional_function: function(){
    var dataEthnicity = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    var responseEthnicity = dataEthnicity['Ethnicity'].includes('Other');
    if(responseEthnicity == true){
      return true;
    } else {
      return false;
    }
  }
};

// political views
var political = {
  type: 'survey-likert',
  questions: itemPolitical,
  post_trial_gap: 500,
  data: {whichWindow: 'demographics'},
  on_finish: function(data){
    var dataPoltical = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
    data.politicalIdeology = dataPoltical['Ideology'];
  }
};

// var futureStudies = {
//     type: 'survey-multi-choice',
//     questions: itemFutureStudies,
//     data: {whichWindow: 'demographics'}
// };
