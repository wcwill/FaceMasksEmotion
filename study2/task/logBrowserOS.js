//** OS and Browser functions in 'index.html' **//

// Record the OS of the computer used to do the experiment
function getOS() {
    var userAgent = window.navigator.userAgent,
        platform = window.navigator.platform,
        macosPlatforms = ['Macintosh', 'MacIntel', 'MacPPC', 'Mac68K'],
        windowsPlatforms = ['Win32', 'Win64', 'Windows', 'WinCE'],
        iosPlatforms = ['iPhone', 'iPad', 'iPod'],
        os = null;
  
    if (macosPlatforms.indexOf(platform) !== -1) {
      os = 'Mac OS';
    } else if (iosPlatforms.indexOf(platform) !== -1) {
      os = 'iOS';
    } else if (windowsPlatforms.indexOf(platform) !== -1) {
      os = 'Windows';
    } else if (/Android/.test(userAgent)) {
      os = 'Android';
    } else if (!os && /Linux/.test(platform)) {
      os = 'Linux';
    }
  
    return os;
  }
  var subjectOS = getOS()

  // Record the browser the participant uses for the experiment
  var navUserAgent = navigator.userAgent;
  var browserName  = navigator.appName;
  var tempNameOffset,tempVersionOffset,tempVersion;

  if ((tempVersionOffset=navUserAgent.indexOf("Opera"))!=-1) {
  browserName = "Opera";
  } else if ((tempVersionOffset=navUserAgent.indexOf("Edge"))!=-1) {
  browserName = "Microsoft Edge";
  } else if ((tempVersionOffset=navUserAgent.indexOf("MSIE"))!=-1) {
  browserName = "Microsoft Internet Explorer";
  } else if ((tempVersionOffset=navUserAgent.indexOf("Trident"))!=-1) {
  browserName = "Internet Explorer 11";
  } else if ((tempVersionOffset=navUserAgent.indexOf("Chrome"))!=-1) {
  browserName = "Chrome";
  } else if ((tempVersionOffset=navUserAgent.indexOf("Safari"))!=-1) {
  browserName = "Safari";
  if ((tempVersionOffset=navUserAgent.indexOf("Version"))!=-1); 
  } else if ((tempVersionOffset=navUserAgent.indexOf("Firefox"))!=-1) {
  browserName = "Firefox";
  } else if ( (tempNameOffset=navUserAgent.lastIndexOf(' ')+1) < (tempVersionOffset=navUserAgent.lastIndexOf('/')) ) {
  browserName = navUserAgent.substring(tempNameOffset,tempVersionOffset);
  if (browserName.toLowerCase()==browserName.toUpperCase()) {
    browserName = navigator.appName;
  }
  };
