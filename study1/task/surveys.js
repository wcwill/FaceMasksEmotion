//** set up surveys presented in 'index.html' **//

// define questionnaire scales
var scaleAQ = [
  "Definitely agree",
  "Slightly agree",
  "Slightly disagree",
  "Definitely disagree"
];

var scaleBEQ = [
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

var scaleIRI = [
  "A Does not describe me well",
  "B",
  "C",
  "D",
  "E Describes me very well"
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

// define questionnaire items
var itemsAQ1 = [{
    prompt: "I prefer to do things on my own rather than with others.",
    name: 'AQ1',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "Other people frequently tell me that what I've said is impolite, even though I think it is polite.",
    name: 'AQ2',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I find social situations easy.",
    name: 'AQ3',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I would rather go to a library than a party.",
    name: 'AQ4',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I find myself drawn more strongly to people than to things.",
    name: 'AQ5',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I enjoy social chit-chat.",
    name: 'AQ6',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "When I talk, it isn't always easy for others to get a word in edgeways.",
    name: 'AQ7',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I find it hard to make new friends.",
    name: 'AQ8',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I frequently find that I don't know how to keep a conversation going.",
    name: 'AQ9',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: 'I find it easy to "read between the lines" when someone is talking to me.',
    name: 'AQ10',
    labels: scaleAQ,
    required: true
  }
];

var itemsAQ2 = [{
    prompt: "I know how to tell if someone listening to me is getting bored.",
    name: 'AQ11',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "When I talk on the phone, I'm not sure when it's my turn to speak.",
    name: 'AQ12',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I am often the last to understand the point of a joke.",
    name: 'AQ13',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I find it easy to work out what someone is thinking or feeling just by looking at their face.",
    name: 'AQ14',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I am good at social chit-chat",
    name: 'AQ15',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "People often tell me that I keep going on and on about the same thing.",
    name: 'AQ16',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I enjoy social occasions.",
    name: 'AQ17',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I find it difficult to work out people's intentions.",
    name: 'AQ18',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I enjoy meeting new people.",
    name: 'AQ19',
    labels: scaleAQ,
    required: true
  },
  {
    prompt: "I am a good diplomat",
    name: 'AQ20',
    labels: scaleAQ,
    required: true
  }
];

var itemsBEQ1 = [{
    prompt: "Whenever I feel positive emotions, people can easily see exactly what I am feeling.",
    name: 'BEQ1',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "I sometimes cry during sad movies.",
    name: 'BEQ2',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "People often do not know what I am feeling.",
    name: 'BEQ3',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "I laugh out loud when someone tells me a joke that I think is funny.",
    name: 'BEQ4',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "It is difficult for me to hide my fear.",
    name: 'BEQ5',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "When I'm happy, my feelings show.",
    name: 'BEQ6',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "My body reacts very strongly to emotional situations.",
    name: 'BEQ7',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "I've learned it is better to suppress my anger than to show it.",
    name: 'BEQ8',
    labels: scaleBEQ,
    required: true
  }
];

var itemsBEQ2 = [{
    prompt: "No matter how nervous or upset I am, I tend to keep a calm exterior.",
    name: 'BEQ9',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "I am an emotionally expressive person.",
    name: 'BEQ10',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "I have strong emotions",
    name: 'BEQ11',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "I am sometimes unable to hide my feelings, even though I would like to.",
    name: 'BEQ12',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "Whenever I feel negative emotions, people can easily see exactly what I am feeling.",
    name: 'BEQ13',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "There have been times when I have not been able to stop crying even though I tried to stop.",
    name: 'BEQ14',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "I experience my emotions very strongly.",
    name: 'BEQ15',
    labels: scaleBEQ,
    required: true
  },
  {
    prompt: "What I'm feeling is written all over my face.",
    name: 'BEQ16',
    labels: scaleBEQ,
    required: true
  }
];

var itemsBFI1 = [{
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

var itemsBFI2 = [{
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

var itemsBFI3 = [{
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

var itemsBFI4 = [{
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

var itemsIRI1 = [{
    prompt: "I often have tender, concerned feelings for people less fortunate than me.",
    name: 'IRI1',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: 'I sometimes find it difficult to see things from the "other guy\'s" point of view.',
    name: 'IRI2',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: "Sometimes I don't feel very sorry for people when they are having problems.",
    name: 'IRI3',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: "I try to look at everybody's side of a disagreement before I make a decision.",
    name: 'IRI4',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: "When I see someone being taken advantage of, I feel kind of protective towards them.",
    name: 'IRI5',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: "I sometimes try to understand my friends better by imagining how things look from their perspective.",
    name: 'IRI6',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: "Other people's mimsfortunes do not usually disturb me a great deal.",
    name: 'IRI7',
    labels: scaleIRI,
    required: true
  }
];

var itemsIRI2 = [{
    prompt: "If I'm sure I'm right about something, I don't waste much time listening to other people's arguments.",
    name: 'IRI8',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: "When I see someone being treated unfairly, I sometimes don't feel very much for them.",
    name: 'IRI9',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: "I am often quite touched by things that I see happen.",
    name: 'IRI10',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: "I believe that there are two sides to every question and try to look at both of them.",
    name: 'IRI11',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: "I would describe myself as a pretty soft-hearted person",
    name: 'IRI12',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: 'When I\'m upset at someone, I usually try to "put myself in his shoes" for a while.',
    name: 'IRI13',
    labels: scaleIRI,
    required: true
  },
  {
    prompt: "Before criticizing somebody, I try to imagine how I would feel if I were in their place.",
    name: 'IRI14',
    labels: scaleIRI,
    required: true
  }
];

var itemsIRQ1 = [{
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

var itemsIRQ2 = [{
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
  prompt: 'Please indicate your gender.',
  name: 'Gender',
  options: ['Male', 'Female', 'Other'],
  required: true
}];

var itemEthnicity = [{
  prompt: "Please enter your race/ethnic affiliation. Check all that apply.",
  name: 'Ethnicity',
  options: ['Indigenous', 'East Asian', 'South Asian', 'Black or African', 'White or Caucasian', 'Hispanic or Latino', 'Middle Eastern', 'Other'],
  required: true
}];

// var itemFutureStudies = [{
//   prompt: 'Is it okay if we contact you about future studies?',
//   name: 'Future',
//   options: ['Yes', 'No'],
//   required: true
// }];

// combine questionnaire items
var itemsAQ = itemsAQ1.concat(itemsAQ2);
var itemsBEQ = itemsBEQ1.concat(itemsBEQ2);
var itemsBFI = itemsBFI1.concat(itemsBFI2, itemsBFI3, itemsBFI4);
var itemsIRI = itemsIRI1.concat(itemsIRI2);
var itemsIRQ = itemsIRQ1.concat(itemsIRQ2);

// combine questionnaire sections
var sectionsAQ = [itemsAQ1, itemsAQ2];
var sectionsBEQ = [itemsBEQ1, itemsBEQ2];
var sectionsBFI = [itemsBFI1, itemsBFI2, itemsBFI3, itemsBFI4];
var sectionsIRI = [itemsIRI1, itemsIRI2];
var sectionsIRQ = [itemsIRQ1, itemsIRQ2];

// define preamble text
var preambleAQ = "<p> Below are a list of statements. Please read each statement very carefully " +
  "and rate how strongly you agree or disagree with it by selecting your answer.</p>"
  "<p> Below are a list of statements. Please read each statement very carefully " +
  "and rate how strongly you agree or disagree with it by selecting your answer.</p>";

var preambleBEQ = "<p> For each statement below, please indicate your agreement or disagreement. " +
  "Do so by choosing the appropriate number from 1 (strongly disagree) to 7 (strongly agree).</p>";

var preambleBFI = "<p> Here are a number of characteristics that may or may not apply to you. " +
  "For example, do you agree that you are someone " +
  "who <i> likes to spend time with others? </i> Please select a number next to each statement " +
  "to indicate the extent to which <b> you agree " +
  "or disagree with that statement </b></p><p> I am someone who...</p>";

var preambleIRI = "<p> The following statements inquire about your thoughts and feelings in a variety " +
  "of situations. For each question indicate how well it describes you by choosing the " +
  "appropriate letter <b> A (Not well), B, C, D, or E (Very Well) </b>. Read each item carefully " +
  "before responding and answer as honeslty as you can.</p>"
  "<p> The following statements inquire about your thoughts and feelings in a variety " +
  "of situations. For each question indicate how well it describes you by choosing the " +
  "appropriate letter <b> A (Not well), B, C, D, or E (Very Well) </b>. Read each item carefully " +
  "before responding and answer as honeslty as you can.</p>";

var preambleIRQ = "<p> Please indicate how much you agree or disagree with each of the following statements.</p>"
  "<p> Please indicate how much you agree or disagree with each of the following statements.</p>";

// set up questionnaires (log responses and attention check within IRQ)
function createAQ() {
  array = []
  // loop through questionnaire sections
  var sectionIndex = Array.apply(null, {length: sectionsAQ.length}).map(Number.call, Number);
  sectionIndex.forEach(function(i) {
    var object = {}
    object['type'] = 'survey-likert'
    object['preamble'] = preambleAQ
    object['questions'] = sectionsAQ[i]
    object['post_trial_gap'] = 500
    object['data'] = {whichWindow: 'questionnaire', whichQ: 'AQ'}
    object['on_finish'] = function(data){
      var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses)
      // loop through items within each questionnaire section
      var itemIndex = Array.apply(null, {length: itemsAQ.length}).map(Number.call, Number);
      itemIndex.forEach(function(j) {
        j++
        data['AQ' + j] = qData['AQ' + j] 
      })
    }
  array.push(object)
  })
  return array
}

function createBEQ() {
  array = []
  // loop through questionnaire sections
  var sectionIndex = Array.apply(null, {length: sectionsBEQ.length}).map(Number.call, Number);
  sectionIndex.forEach(function(i) {
    var object = {}
    object['type'] = 'survey-likert'
    object['preamble'] = preambleBEQ
    object['questions'] = sectionsBEQ[i]
    object['post_trial_gap'] = 500
    object['data'] = {whichWindow: 'questionnaire', whichQ: 'BEQ'}
    object['on_finish'] = function(data){
      var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses)
      // loop through items within each questionnaire section
      var itemIndex = Array.apply(null, {length: itemsBEQ.length}).map(Number.call, Number);
      itemIndex.forEach(function(j) {
        j++
        data['BEQ' + j] = qData['BEQ' + j] 
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

function createIRI() {
  array = []
  // loop through questionnaire sections
  var sectionIndex = Array.apply(null, {length: sectionsIRI.length}).map(Number.call, Number);
  sectionIndex.forEach(function(i) {
    var object = {}
    object['type'] = 'survey-likert'
    object['preamble'] = preambleIRI
    object['questions'] = sectionsIRI[i]
    object['post_trial_gap'] = 500
    object['data'] = {whichWindow: 'questionnaire', whichQ: 'IRI'}
    object['on_finish'] = function(data){
      var qData = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses)
      // loop through items within each questionnaire section
      var itemIndex = Array.apply(null, {length: itemsIRI.length}).map(Number.call, Number);
      itemIndex.forEach(function(j) {
        j++
        data['IRI' + j] = qData['IRI' + j] 
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

// create questionnaires
var AQ = createAQ();
var BEQ = createBEQ();
var BFI = createBFI();
var IRI = createIRI();
var IRQ = createIRQ();

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

// var futureStudies = {
//     type: 'survey-multi-choice',
//     questions: itemFutureStudies,
//     data: {whichWindow: 'demographics'}
// };
