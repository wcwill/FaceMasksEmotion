//** set up block text presented in 'index.html' **//

// consent form
var consentForm = {
  type: "html-button-response",
  choices: ['Continue'],
  post_trial_gap: 500,
  timeline: [
    {stimulus: "<p>Welcome to the experiment! Please click continue to begin.</p>"},
    {stimulus: "<p>Before starting the experimental task, we ask that you review the following consent form.</p>"},
    {stimulus: "<p><b><u>ONLINE CONSENT FORM (PAGE 1 OF 3)</u></b><p>"+
      "<p><b>Why you are being invited to take part in a research study?</b></p>"+
        "<p>We invite you to partake in this research study to understand how attitudes shape decisions related to attractiveness.</p>"+
      "<p><b>Who can I talk to?</b></p>"+
        "<p>If you have questions, concerns, or complaints, or think the research has hurt you, contact the research team at vinod.venkatraman@temple.edu or call 215-204-8177. Our physical address is Alter Hall A562, 1801 Liacouras Walk, Philadelphia, PA 19122.</p>"+
        "<p>This research has been reviewed and approved by an Institutional Review Board. You may talk to them at (215) 707-3390 or e-mail them at: irb@temple.edu for any of the following:</p>"+
          "<p><ul style='list-style-type:none'>"+
            "<li>Your questions, concerns, or complaints are not being answered by the research team.</li>"+
            "<li>You cannot reach the research team.</li>"+
            "<li>You want to talk to someone besides the research team.</li>"+
            "<li>You have questions about your rights as a research subject.</li>"+
            "<li>You want to get information or provide input about this research.</li>"+
          "</ul><p>"},
    {stimulus: "<p><b><u>ONLINE CONSENT FORM (PAGE 2 OF 3)</u></b><p>"+
      "<p><b>Why are we doing this research?</b></p>"+
        "<p>The purpose of the research is to better understand how different attitudes shape people's behavior and decision preferences.</p>"+
      "<p><b>How long will the research last?</b></p>"+
        "<p>Your participation in this study will last for one session with the duration of no longer than 1 hour.</p>"+
      "<p><b>What happens if I say yes, I want to be in this research?</b></p>"+
        "<p>After your consent to participate, you will be asked to indicate your preferences in a series of trials. The specific details and instructions for the task will be communicated by the experimenter prior to the start of the study. Please follow these instructions to the best of your ability. There will be attention checks throughout the study to ensure that you are paying attention and the data collected is valid.</p>"+
        "<p>We will also collect basic demographics from you such as income, education, purchasing behavior, political and altruistic attitudes, and decision-making behavior (such as sensitivity to risk). Remember that all the decisions and choices that you make during this study are your own opinions and preferences. There are no right or wrong answers. Please answer honestly and to the best of your abilities. Participating in this study is no more risky than using an everyday computer.</p>"},
    {stimulus: "<p><b><u>ONLINE CONSENT FORM (PAGE 3 OF 3)</u></b><p>"+
      "<p><b>What happens if I say no, I do not want to be in this research?</b></p>"+
        "<p>You may decide not to take part in the research and it will not be held against you. It will in no way affect your relationship with the study investigators.</p>"+
      "<p><b>What happens if I say yes, but I change my mind later?</b></p>"+
        "<p>If you agree to take part in the research, you can stop at any time, and it will not be held against you. Withdrawal from study or failure to participate will not have any academic consequences to you if you are a student. Furthermore, if you are a student, there are no risks to your student career.</p>"+
      "<p><b>What happens to the information we collect?</b></p>"+
        "<p>Efforts will be made to limit the disclosure of your personal information, including research study records, to people who have a need to review this information. However, the study team cannot promise complete secrecy. For example, although the study team has put in safeguards to protect your information, there is always a potential risk of loss of confidentiality. Temple University IRB may inspect and copy your information to make sure that the study team is following the rules and regulations regarding research and the protection of human subjects.</p>"+
      "<p><b>Stipend/Reimbursement</b></p>"+
        "<p>If you agree to take part in this research, you will be compensated through the survey panel (e.g., mTurk, Qualtrics, SONA) through which you registered, after you complete the survey. However, you should have been informed about the estimated duration and exact compensation prior to registering for the study on the corresponding online forum. If not, you may discontinue at this point.</p>"},
    {stimulus: "<p>By clicking <b>'Continue'</b> below, you <b>consent</b> to participating in this research.</p>"}
  ]
};

// general instructions
var generalInstructs = {
  type: "html-button-response",
  choices: ['Continue'],
  post_trial_gap: 500,
  timeline: [
    {stimulus: "<p>In the first part of the experiment, you will view a series of faces and rate whether or not each face looks angry, disgusted, fearful, happy, sad, or surprised.</p>"+"<p>Sometimes you will be able to see the entire face. Other times you may only be able to see the top or the bottom half of the face.</p>"},
    {stimulus: "<p>Before viewing each face, you will briefly see a + sign display in the center of the screen.</p>"+"<p>You will then see a face appear and you will press one of two keys to indicate whether or not the face is showing a certain emotion.</p>"},
    {stimulus: "<p>Please respond as quickly and accurately as possible when you see each face appear.</p>"+"<p>Once the computer has logged your response, the face will disappear and you will see the next + sign display.</p>"},
    {stimulus: "<p>You will complete six rounds of the task and each round will last about 3-5 minutes.</p>"+"<p>Please press continue when you are ready to begin.</p>"}
  ]
};

// questionnaire instructions
var qInstructs = {
  type: "html-button-response",
  choices: ['Continue'],
  post_trial_gap: 500,
  stimulus: "<p>Thank you for completing the main task.</p>"+
  "<p>In the second part of the experiment, you will respond to a series of questions.</p>"+
  "<p>Please press continue when you are ready to begin.</p>"
};

// debrief
var debrief = {
  type: "html-button-response",
  choices: ['Continue'],
  post_trial_gap: 500,
  timeline: [
    {stimulus: "<p>Thank you for participating in our experiment!</p>"+"<p> The purpose of this experiment is to study how people perceive partially visible facial expressions of emotion.</p>"+"<p>Your participation could help provide insight into how wearing face coverings influences the communication of distinct emotions.</p>"},
    {stimulus: "<p>In the first part of the experiment, you rated whether or not a series of faces showed various emotions.</p>"+"<p>Your ratings may help us understand how quickly and accurately individuals perceive facial expressions of different emotions when they are only partially visible.</p>"},
    {stimulus: "<p>In the second part of the experiment, you answered a set of questionnaires that assessed certain personality traits or dispositions.</p>"+"<p>These questionnaires are meant to measure differences between individuals, which may be related to how they rated faces in the first part of the study.</p>"},
    {stimulus: "<p>Click continue to finish the experiment and receive your secret completion code.</p>"+"<p>Please enter this code in the HIT window and submit the HIT <b>before</b> you close this window.</p>"}
  ]
};
