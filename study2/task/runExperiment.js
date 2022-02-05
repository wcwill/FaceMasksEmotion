  //** faceMask study 2 experiment presented in 'index.html' **//

  //** SET UP EXPERIMENT **//
  // get mTurk ID info
  var turkInfo = jsPsych.turk.turkInfo();

  // get Ipsos ID
  // var ipsosID = jsPsych.data.getURLVariable('uid');

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

  // log mTurk/Ipsos info, key pairing, and OS/browser for every trial in the jsPsych data
  jsPsych.data.addProperties({
    workerID: turkInfo.workerId,
    hitID: turkInfo.hitId,
    assignmentID: turkInfo.assignmentId,
    // ipsosID: ipsosID,
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
  
  // randomize face list orders for emotion rating task (all faces) and person rating task (only unmasked and lower-masked faces)
  var shuffleFaceListsEmotionRating = jsPsych.randomization.shuffle(allFaceLists);
  var allFaceListsPersonRating = [faceListLX1, faceListLX2, faceListLX3, faceListLX4, faceListLX5, faceListLX6];
  var shuffleFaceListsPersonRating = jsPsych.randomization.shuffle(allFaceListsPersonRating);

  // randomize emotion rating order
  var allEmotionRatings = ['angry', 'disgusted', 'fearful', 'happy', 'sad', 'surprised'];
  var shuffleEmotionRatings = jsPsych.randomization.shuffle(allEmotionRatings);

  // randomize person rating order
  var allPersonRatings = ['likable', 'trustworthy', 'warm'];
  var shufflePersonRatings = jsPsych.randomization.shuffle(allPersonRatings);

  // create timeline 
  var timeline = [];
  
  // initialize connection with pavlovia.org (enable in final code)
  // var pavloviaInit = {
  //    type: "pavlovia",
  //    command: "init"
  // };
  // timeline.push(pavloviaInit);

  // consent form and emotion rating task instructions
  timeline.push(consentForm);
  timeline.push(emotionRatingInstructs);

  // fixation presentation window
  var fixationWindow = {
    type: 'html-keyboard-response',
    stimulus: '+',
    choices: jsPsych.NO_KEYS,
    trial_duration: 1000,
    data: {whichWindow: 'fixation'}
  };


  //** EMOTION RATING TASK **//
  // initialize trial counter
  var emotionRatingTrialCounter = 0;

  // emotion rating instructions check
  var itemCheckEmotionRating = [{
    prompt: 'Which emotion are you rating for this round?',
    name: 'CheckEmotionRating',
    options: ['angry', 'disgusted', 'fearful', 'happy', 'sad', 'surprised'],
    required: true
  }];

  // loop through blocks
  var blockIndexEmotionRating = Array.apply(null, {length: shuffleEmotionRatings.length}).map(Number.call, Number);
  blockIndexEmotionRating.forEach(function(i) {

    // instructions for this block depending on emotion rating and key pairing
    var blockInstructsEmotionRating = {
      type: "html-button-response",
      choices: ['Continue'],
      post_trial_gap: 500,
      stimulus: "<p>This round, you will rate whether each face looks <b>"+shuffleEmotionRatings[i]+"</b> or not.</p>" + 
      "<p>If the face looks <b>"+shuffleEmotionRatings[i]+"</b>, press the letter <b>"+keyYes+"</b>. " + 
      "If the face does <b>NOT</b> look "+shuffleEmotionRatings[i]+", press the letter <b>"+keyNo+"</b>.</p>" + 
      "<p>Please respond as quickly and accurately as possible when you see each face appear.</p>" + 
      "<p>Click continue when you are ready to start this round.</p>"
    };

    var checkEmotionRating = {
      type: 'survey-multi-choice',
      questions: itemCheckEmotionRating,
      post_trial_gap: 500,
      data: {whichWindow: 'instructionChecks'},
      on_finish: function(data) {
        var dataCheckEmotionRating = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
        data.checkRatingAccuracy = dataCheckEmotionRating["CheckEmotionRating"]==shuffleEmotionRatings[i];
      }
    };

    // key pairing instructions check
    var checkEmotionRatingKey = {
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
    var ifInstructsCheckEmotionRating = {
      timeline: [blockInstructsEmotionRating, checkEmotionRating, checkEmotionRatingKey],
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
    
    timeline.push(ifInstructsCheckEmotionRating);
        
    // face presentation window
    var faceWindowEmotionRating = {
      type: "image-keyboard-response",
      stimulus: jsPsych.timelineVariable('image'),
      choices: ['q', 'p'],
      data: jsPsych.timelineVariable('data'),
      on_finish: function(data){
        // log block number, emotion rating, and trial number
        data.block = parseInt(i);
        data.emotionRating = shuffleEmotionRatings[i];
        data.trial = emotionRatingTrialCounter;
        emotionRatingTrialCounter++;

        // log unique face code, model #, model ethnicity, model sex, expression, and mask
        var image = jsPsych.data.getLastTrialData().values()[0].stimulus;
        data.face = image.substr(7,8);
        data.model = image.substr(7,4);
        data.ethnicity = image.substr(7,1)
        data.sex = image.substr(8,1);
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
    var trialsEmotionRating = {
      timeline: [fixationWindow, faceWindowEmotionRating],
      timeline_variables: shuffleFaceListsEmotionRating[i],
      repetitions: 1,
      randomize_order: true
    };
    
    timeline.push(trialsEmotionRating);
  });


  //** PERSON RATING TASK **//
  // person rating task instructions
  timeline.push(personRatingInstructs);

  // initialize trial counter
  var personRatingTrialCounter = 0;

  // person rating instructions check
  var itemCheckPersonRating = [{
    prompt: 'Which quality are you rating for this round?',
    name: 'CheckPersonRating',
    options: ['likable', 'trustworthy', 'warm'],
    required: true
  }];

  // loop through blocks
  var blockIndexPersonRating = Array.apply(null, {length: shufflePersonRatings.length}).map(Number.call, Number);
  blockIndexPersonRating.forEach(function(i) {

    // instructions for this block depending on person rating and key pairing
    var blockInstructsPersonRating = {
      type: "html-button-response",
      choices: ['Continue'],
      post_trial_gap: 500,
      stimulus: "<p>This round, you will rate whether each face looks <b>"+shufflePersonRatings[i]+"</b> or not.</p>" + 
      "<p>If the face looks <b>"+shufflePersonRatings[i]+"</b>, press the letter <b>"+keyYes+"</b>. " + 
      "If the face does <b>NOT</b> look "+shufflePersonRatings[i]+", press the letter <b>"+keyNo+"</b>.</p>" + 
      "<p>Please respond as quickly and accurately as possible when you see each face appear.</p>" + 
      "<p>Click continue when you are ready to start this round.</p>"
    };

    var checkPersonRating = {
      type: 'survey-multi-choice',
      questions: itemCheckPersonRating,
      post_trial_gap: 500,
      data: {whichWindow: 'instructionChecks'},
      on_finish: function(data) {
        var dataCheckPersonRating = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
        data.checkRatingAccuracy = dataCheckPersonRating["CheckPersonRating"]==shufflePersonRatings[i];
      }
    };

    // key pairing instructions check
    var checkPersonRatingKey = {
      type: "survey-html-form",
      preamble: '<p> Which key do you press... </p>',
      html: '<p> ...if the face looks <b>'+shufflePersonRatings[i]+'</b> <input name="checkKeyYes" type="text" /></p><p> ...if the face does <b>NOT</b> look '+shufflePersonRatings[i]+' <input name="checkKeyNo" type="text" /></p>',
      post_trial_gap: 500,
      data: {whichWindow: 'instructionChecks'},
      on_finish: function(data) {
        var dataCheckKeys = JSON.parse(jsPsych.data.getLastTrialData().values()[0].responses);
        data.checkKeyYesAccuracy = dataCheckKeys["checkKeyYes"].toUpperCase()==keyYes;
        data.checkKeyNoAccuracy = dataCheckKeys["checkKeyNo"].toUpperCase()==keyNo;
      }
    };

    // correctly ID person rating and key pairing to advance (otherwise repeat block instructions)
    var ifInstructsCheckPersonRating = {
      timeline: [blockInstructsPersonRating, checkPersonRating, checkPersonRatingKey],
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
    
    timeline.push(ifInstructsCheckPersonRating);

    // face presentation window
    var faceWindowPersonRating = {
      type: "image-keyboard-response",
      stimulus: jsPsych.timelineVariable('image'),
      choices: ['q', 'p'],
      data: jsPsych.timelineVariable('data'),
      on_finish: function(data){
        // log block number, person rating, and trial number
        data.block = parseInt(i); // UPDATE
        data.personRating = shufflePersonRatings[i];
        data.trial = personRatingTrialCounter;
        personRatingTrialCounter++;

        // log unique face code, model #, model ethnicity, model sex, expression, and mask
        var image = jsPsych.data.getLastTrialData().values()[0].stimulus;
        data.face = image.substr(7,8);
        data.model = image.substr(7,4);
        data.ethnicity = image.substr(7,1)
        data.sex = image.substr(8,1);
        data.expression = image.substr(12,1);
        data.mask = image.substr(14,1);

        // log key presses as yes/no (1/0) emotion ratings for this key pairing
        if (thisKeyPair == 0) {
          if (data.key_press == 80) { // keyCode for 'P'
            data.ratePerson = 1;
          } else if (data.key_press == 81) { // keyCode for 'Q'
            data.ratePerson = 0;
          }
        } else if (thisKeyPair == 1) {
          if (data.key_press == 81) { // keyCode for 'Q'
            data.ratePerson = 1;
          } else if (data.key_press == 80) { // keyCode for 'P'
            data.ratePerson = 0;
          }
        }
      }
    };

    // generate trials for this block and randomize trial order
    var trialsPersonRating = {
      timeline: [fixationWindow, faceWindowPersonRating],
      timeline_variables: shuffleFaceListsPersonRating[i],
      repetitions: 1,
      randomize_order: true
    };
    
    timeline.push(trialsPersonRating);
  });


  //** QUESTIONNAIRES **//
  // questionnaire instructions
  timeline.push(qInstructs);

  // Ipsos questions
  timeline.push(ipsos1);
  timeline.push(ifNodeIpsos1a);
  timeline.push(ifNodeIpsos1b);

  // randomize order of IRQ sections
  var shuffleIRQ = jsPsych.randomization.shuffle(IRQ);

  // randomize questionnaire order
  var Qs = [AQC, BEQS, BFI, shuffleIRQ, INDCOL];
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
  timeline.push(political);
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
    exclusions: {min_width: 800, min_height: 600}, // restrict screen resolution (enable in final code)
    show_progress_bar: true,
    on_finish: function() {
      jsPsych.getDisplayElement().innerHTML = "<p>Secret Completion Code: <b>6HUEU752WK</b></p>"+"<p>Please enter this code in the HIT window and submit the HIT <b>before</b> you close this window.</p>"+"<p>Thank you again for participating and please feel free to contact us at card@temple.edu if you have any questions.</p>";
      // jsPsych.getDisplayElement().innerHTML = "<p>Thank you again for participating and please feel free to contact us at card@temple.edu if you have any questions.</p>"+"<p>In 10 seconds, you will be automatically redirected back to Ipsos. Please <b>do not</b> close this window.</p>";
      // jsPsych.data.displayData(); // display data (disable in final code)
      // jsPsych.data.get().localSave('csv','mydata.csv'); // save data (disable in final code)
      // setTimeout(function(){window.location.replace("https://amp.ipsosinteractive.com/resume.aspx?st=3&uid="+ipsosID);}, 10000); // redirect back to Ipsos (enable in final code)
    }
  });
