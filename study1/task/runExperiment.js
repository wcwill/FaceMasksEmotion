  //** faceMask experiment presented in 'index.html' **//

  //** SET UP EXPERIMENT **//
  // get mTurk ID info
  var turkInfo = jsPsych.turk.turkInfo();

  // generate a random subject ID with 15 characters
  var thisSubject = jsPsych.randomization.randomID(15);

  // randomly assign key pairing for this subject 
  var thisKeyPair = jsPsych.randomization.sampleWithoutReplacement([0, 1], 1)[0];

  if (thisKeyPair == 0) {
    var keyYes = 'P';
    var keyNo = 'Q';
  } else if (thisKeyPair == 1) {
    var keyYes = 'Q';
    var keyNo = 'P';
  }

  // log mTurk info, key pairing, and OS/browser for every trial in the jsPsych data
  jsPsych.data.addProperties({
    workerID: turkInfo.workerId,
    hitID: turkInfo.hitId,
    assignmentID: turkInfo.assignmentId,
    subject: thisSubject,
    keyPair: thisKeyPair,
    subjectOS: subjectOS,
    subjectBrowser: browserName
  });

  // pre-load image files    
  var allFaceLists = [faceList1, faceList2, faceList3, faceList4, faceList5, faceList6];
  var preloadImages = [];
  
  // loop through face lists (using IE-compatible for-loop)
  var faceListIndex = Array.apply(null, {length: allFaceLists.length}).map(Number.call, Number);
  faceListIndex.forEach(function(i) {
    // loop through faces within each face list
    var faceIndex = Array.apply(null, {length: allFaceLists[i].length}).map(Number.call, Number);
    faceIndex.forEach(function(j) {
      // preload each face image
      preloadImages.push(allFaceLists[i][j].image);
    });
  });
  
  // randomize face list order
  var shuffleFaceLists = jsPsych.randomization.shuffle(allFaceLists);

  // randomize emotion rating order
  var allEmotionRatings = ['angry', 'disgusted', 'fearful', 'happy', 'sad', 'surprised'];
  var shuffleEmotionRatings = jsPsych.randomization.shuffle(allEmotionRatings);

  // create timeline 
  var timeline = [];
  
  // initialize connection with pavlovia.org (enable in final code)
  // var pavloviaInit = {
  //    type: "pavlovia",
  //    command: "init"
  // };
  // timeline.push(pavloviaInit);

  // consent form and general instructions
  timeline.push(consentForm);
  timeline.push(generalInstructs);


  //** MAIN TASK **//
  // emotion rating instructions check
  var itemCheckRating = [{
    prompt: 'Which emotion are you rating for this round?',
    name: 'CheckRating',
    options: ['angry', 'disgusted', 'fearful', 'happy', 'sad', 'surprised'],
    required: true
  }];

  // fixation presentation window
  var fixationWindow = {
    type: 'html-keyboard-response',
    stimulus: '+',
    choices: jsPsych.NO_KEYS,
    trial_duration: 1000,
    data: {whichWindow: 'fixation'}
  };

  // initialize trial counter
  var trialCounter = 0;

  // loop through blocks
  var blockIndex = Array.apply(null, {length: shuffleFaceLists.length}).map(Number.call, Number);
  blockIndex.forEach(function(i) {

    // instructions for this block depending on emotion rating and key pairing
    var blockInstructs = {
      type: "html-button-response",
      choices: ['Continue'],
      post_trial_gap: 500,
      stimulus: "<p>This round, you will rate whether each face looks <b>"+shuffleEmotionRatings[i]+"</b> or not.</p>" + 
      "<p>If the face looks <b>"+shuffleEmotionRatings[i]+"</b>, press the letter <b>"+keyYes+"</b>. " + 
      "If the face does <b>NOT</b> look "+shuffleEmotionRatings[i]+", press the letter <b>"+keyNo+"</b>.</p>" + 
      "<p>Please respond as quickly and accurately as possible when you see each face appear.</p>" + 
      "<p>Click continue when you are ready to start this round.</p>"
    };

    var checkRating = {
      type: 'survey-multi-choice',
      questions: itemCheckRating,
      post_trial_gap: 500,
      data: {whichWindow: 'instructionChecks'},
      on_finish: function(data) {
        var dataCheckRating = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
        data.checkRatingAccuracy = dataCheckRating["CheckRating"]==shuffleEmotionRatings[i];
      }
    };

    // key pairing instructions check
    var checkKey = {
      type: "survey-html-form",
      preamble: '<p> Which key do you press... </p>',
      html: '<p> ...if the face looks <b>'+shuffleEmotionRatings[i]+'</b> <input name="checkKeyYes" type="text" /></p><p> ...if the face does <b>NOT</b> look '+shuffleEmotionRatings[i]+' <input name="checkKeyNo" type="text" /></p>',
      post_trial_gap: 500,
      data: {whichWindow: 'instructionChecks'},
      on_finish: function(data) {
        var dataCheckKeys = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
        data.checkKeyYesAccuracy = dataCheckKeys["checkKeyYes"].toUpperCase()==keyYes;
        data.checkKeyNoAccuracy = dataCheckKeys["checkKeyNo"].toUpperCase()==keyNo;
      }
    };

    // correctly ID emotion rating and key pairing to advance (otherwise repeat block instructions)
    var ifInstructionsCheck = {
      timeline: [blockInstructs, checkRating, checkKey],
      loop_function: function(data) {
        var checkRatingAccuracy = JSON.parse(jsPsych.data.get().last(2).filter({trial_type: 'survey-multi-choice'}).values()[0].checkRatingAccuracy);
        var checkKeyYesAccuracy = JSON.parse(jsPsych.data.get().last(1).filter({trial_type: 'survey-html-form'}).values()[0].checkKeyYesAccuracy);
        var checkKeyNoAccuracy = JSON.parse(jsPsych.data.get().last(1).filter({trial_type: 'survey-html-form'}).values()[0].checkKeyNoAccuracy);
        
        if(checkRatingAccuracy && checkKeyYesAccuracy && checkKeyNoAccuracy) {
            return false;
        } else {
            return true;
        }
      }
    };
    timeline.push(ifInstructionsCheck);
        
    // face presentation window
    var faceWindow = {
      type: "image-keyboard-response",
      stimulus: jsPsych.timelineVariable('image'),
      choices: ['q', 'p'],
      data: jsPsych.timelineVariable('data'),
      on_finish: function(data){
        // log block number, emotion rating, and trial number
        data.block = parseInt(i);
        data.emotionRating = shuffleEmotionRatings[i];
        data.trial = trialCounter;
        trialCounter++;

        // log face image, model #, model sex, expression, and mask
        var image = jsPsych.data.getLastTrialData().values()[0].stimulus;
        data.face = image.substr(7,8);
        data.model = image.substr(7,2);
        data.sex = image.substr(10,1);
        data.expression = image.substr(12,1);
        data.mask = image.substr(14,1);

        // log key presses as yes/no (1/0) emotion ratings for this key pairing
        if (thisKeyPair == 0) {
          if (data.key_press == 80) { // keyCode for 'P'
            data.rateEmotion = 1;
          } else if (data.key_press == 81) { // keyCode for 'Q'
            data.rateEmotion = 0;
          }
        } else if (thisKeyPair == 1) {
          if (data.key_press == 81) { // keyCode for 'Q'
            data.rateEmotion = 1;
          } else if (data.key_press == 80) { // keyCode for 'P'
            data.rateEmotion = 0;
          }
        }
      }
    };

    // generate trials for this block and randomize trial order
    var trials = {
      timeline: [fixationWindow, faceWindow],
      timeline_variables: shuffleFaceLists[i],
      repetitions: 1,
      randomize_order: true
    };
    timeline.push(trials);
  
  });


  //** QUESTIONNAIRES **//
  // questionnaire instructions
  timeline.push(qInstructs);

  // randomize order of IRQ sections
  var shuffleIRQ = jsPsych.randomization.shuffle(IRQ);

  // randomize questionnaire order
  var Qs = [AQ, BEQ, BFI, IRI, shuffleIRQ];
  var shuffleQs = jsPsych.randomization.shuffle(Qs);

  // loop through questionnaires
  var qIndex = Array.apply(null, {length: shuffleQs.length}).map(Number.call, Number);
  qIndex.forEach(function(i) {
    // loop through sections within each questionnaire
    var qSectionIndex = Array.apply(null, {length: shuffleQs[i].length}).map(Number.call, Number);
    qSectionIndex.forEach(function(j) {
      // push questionnaire section
      timeline.push(shuffleQs[i][j]);
    });
  });

  // demographics
  timeline.push(demographics);
  timeline.push(gender);
  timeline.push(ifNodeGenderOther);
  timeline.push(ethnicity);
  timeline.push(ifNodeEthnicityOther);
  // timeline.push(futureStudies);

  // debrief
  timeline.push(debrief);

  // finish connection with pavlovia.org (enable in final code)
  // var pavloviaFinish = {
  //   type: "pavlovia",
  //   command: "finish"
  // };
  // timeline.push(pavloviaFinish);


  //** START EXPERIMENT **//
  jsPsych.init({
    timeline: timeline,
    preload_images: preloadImages,
    // exclusions: {min_width: 800, min_height: 600}, // restrict screen resolution (enable in final code)
    show_progress_bar: true,
    on_finish: function() {
      jsPsych.getDisplayElement().innerHTML = "<p>Secret Completion Code: <b>FK7Y8NPAJ9</b></p>"+"<p>Please enter this code in the HIT window and submit the HIT <b>before</b> you close this window.</p>"+"<p>Thank you again for participating and please feel free to contact us at card@temple.edu if you have any questions.</p>";
      // jsPsych.data.displayData(); // save data (disable in final code)
      // jsPsych.data.get().localSave('csv','mydata.csv'); // display data (disable in final code)
    }
  });
