<!--[CDATA[
/*** YstokHelp - Master script
 *** Copyright (c) 2009-2012 Dr. Dmitriy Ivanov. All rights reserved.
 *** http://lisp.ystok.ru/yhelp/
 ***
 *** Functions called on loading the framest and from the HEAD of navigation or content pages.
 *** Signature:
 ***   function navi_onopen( level, cont )
 ***   function cont_onopen( level, navi )
 *** Args: 
 ***   level	  Level within directory structure, by default, 0 or null means top-level.
 ***   navi/cont  "name.ext" or other pathname relative to the current page;
 ***		  null or not specified means "do not pass".
 *** Purpose:
 ***   If no frameset loaded, we restore it and accompany with query component
 ***   of the original URI requested:
 ***	  path/frameset.html?cont=XXXX&navi=YYYY
 *** NB: Passing a non-null second argument to cont_onopen() is discouraged.
 *** NB: There is no way to hide the address or status bar of the current window
 ***	 due to general browser security resctrictions.
 ***/

var FRAMESET_NAMEEXT = "help.html";
// Localize these
var HIDE_LEFT_TITLE = "Hide navigation frame";
var SHOW_LEFT_TITLE = "Show navigation frame";

var agt = navigator.userAgent.toLowerCase(); 
var is_major = parseInt(navigator.appVersion);
var is_nav  = (agt.indexOf('mozilla') != -1) && (agt.indexOf('spoofer') == -1)
	    && (agt.indexOf('compatible') == -1)
var is_nav4	= is_nav && (is_major == 4);
var is_nav4up	= is_nav && (is_major >= 4);
var is_ie	= (agt.indexOf("msie") != -1);

function getExpDate(days, hours, minutes)
/** Helper to retrieve an expiration date in proper format.
 ** Args: Required numbers of days, hours, and minutes from now you want the cookie to expire.
 **       Pass negatives a past date or zeros where appropriate.
 **/
{ var expDate = new Date();
  if( typeof days == "number" && typeof hours == "number" &&  typeof hours == "number")
  { expDate.setDate(expDate.getDate() + parseInt(days));
    expDate.setHours(expDate.getHours() + parseInt(hours));
    expDate.setMinutes(expDate.getMinutes() + parseInt(minutes));
    return expDate.toGMTString();
} }

/*** Bill Dortch GetCookieVal and GetCookie routines (pairs are separated by "; ")
 *** NB1: wrt top-level document.
 *** NB2: If empty string, always return null instead, otherwise unescape it.
 ***/
function GetCookieVal( c, start )
{ var end = c.indexOf(";", start);
  if( end == -1 ) end = c.length;
  if( start == end ) return null;
  else return unescape(c.substring(start, end));
}

function GetTopCookie( name )
{ var arg = name + "=", alen = arg.length, c = top.document.cookie;
  var j;
  for( var clen = c.length, i = 0; i < clen; )
  { if( c.substring(i, (j = i + alen)) == arg ) return GetCookieVal(c, j);
    if( (i = c.indexOf(" ", i) + 1) == 0 ) break; 
  }
  return null;
}

function SetTopCookie( name, value, expires, path, domain, secure )
{ top.document.cookie = name + "=" + escape(value) +
	((expires) ? "; expires=" + expires : "") +
        ((path) ? "; path=" + path : "") +
        ((domain) ? "; domain=" + domain : "") +
        ((secure) ? "; secure" : "");
}

/*************************************** libDHTML **************************************/
function parse_search_alist()
 // Create an alist (or hash-table) from the query component of the current URI
{ var alist = new Object();
  if( location.search.substr )
  { var s = unescape(location.search.substr(1));
    if( s )
    { s = s.split("&");
      for( pair = new Array(), i = 0; i < s.length; i++ )
      { pair = s[i].split("=");
        alist[pair[0]] = pair[1];
  } } }
  return alist;
}

function upper_dir( level, uri )
 // Helper stipping everying after the path, a la pathname-location if level=0
{ var s = "" + uri;
  var pos = s.lastIndexOf("/");			// absolute directory
  var end;
  if( pos != -1 )
    while( level-- > 0 )
    { end = s.lastIndexOf("/", pos-1);		// search from end - end index included!
      if( end == -1 ) break;
      pos = end;
    }
  return s.substring(0, pos+1);
}

function nav_find_layer( doc, name )
 // Search for nested Navugator 4 layer from string name
{ var elem;
  for( var i = 0; i < doc.layers.length; i++ )
  { if( doc.layers[i].name == name) { elem = doc.layers[i]; break; }
    if( doc.layers[i].document.layers.length > 0 )	// recurse into nested layers if necessary
      elem = nav_find_layer(document.layers[i].document, name);
  }
  return elem;
}
   
function find_element( obj )
 // Generic: Convert object name or reference into a valid element object reference
{   var elem;
    if( typeof obj == "string" )
    { if( document.getElementById ) elem = document.getElementById(obj);
      else if( is_ie ) elem = document.all(obj);
      else if( is_nav4 ) elem = nav_find_layer(document, obj);
    }
    else elem = obj;			// pass through object reference
    return elem;
}

function add_event( elem, evtType, fun, capture )
{ if( elem.addEventListener )
    elem.addEventListener(evtType, fun, (capture || false));
  else if( elem.attachEvent ) 
    elem.attachEvent("on" + evtType, fun);
  else elem["on" + evtType] = fun;	// for IE/Mac, Nav4, and older
}

function remove_event( elem, evtType, fun, capture )
{ if( elem.removeEventListener )
    elem.removeEventListener(evtType, fun, (capture || false));
  else if( elem.attachEvent )
    elem.detachEvent("on" + evtType, fun);
  else elem["on" + evtType] = null; 	// for IE/Mac, Nav4, and older
}

function add_onload_event( fun )
{ if( window.addEventListener || window.attachEvent ) 
    add_event(window,"load", fun, false);
  else
  { var old = window.onload; 		// make closure over the old onload handler
    if( old ) window.onload = function() { old(); fun(); }
    else window.onload = fun;
} }
/************************************* End of libDHTML *************************************/

/** Globals for saving left col width and state
 **/
var leftFrameWidth = null;	// ::= "33%" | "0"
var leftFrameHidden = null;	// ::= false | true
   
function frameset_onload()
 // Called by: the frameset onload event
{ if( location.search )
  { var alist = parse_search_alist();
    var val = alist["navi"];
    if( val ) self.navi.location.href = val;
    val = alist["cont"];
    if( val ) self.cont.location.href = val;
} }

function navi_onopen( level, cont )
 // Called by: HEAD of naviagion pages
 // Nav workaround: During print, it loads frame content into invisible frame of zero width
{ if( window == parent && (!is_nav4up || window.innerWidth != 0) )
  { var frameset_uri = upper_dir(level, location.href) + FRAMESET_NAMEEXT
		+ "?navi=" + escape(location.href)
		+ (cont ? ("&cont=" + escape(upper_dir(0, location.href) + cont)) : "");
    if( location.replace )			// if replace is available, use it 
      location.replace(frameset_uri);		// to keep current page out of history
    else location.href = frameset_uri;
  }
  else add_event(window, "resize", navi_onresize);
}

function cont_onopen( level, navi )
 // Called by: HEAD of topic pages
{ if( (window == parent) && (!is_nav4up || window.innerWidth != 0) )
  { var frameset_uri = upper_dir(level, location.href) + FRAMESET_NAMEEXT
		+ "?cont=" + escape(location.href)
		+ (navi ? ("&navi=" + escape(upper_dir(0, location.href) + navi)) : "");
    if( location.replace )			// if replace is available, use it 
      location.replace(frameset_uri);		// to keep current page out of history
    else location.href = frameset_uri;
  }
  else add_onload_event(cont_onload);
}

function ResizeLeftFrame( hide, w, oninit )
 // Args: oninit  If true, do not save in cookie
{ var frameset = document.getElementById("yh_frameset");
  if( hide )
  { if( leftFrameWidth === null )
      if( oninit ) leftFrameWidth = GetTopCookie("leftFrameWidth");
      else
      { if( is_nav4up ) leftFrameWidth = window.frames[0].innerWidth;
        else leftFrameWidth = frameset.cols.substr(0, frameset.cols.indexOf(","));
	SetTopCookie("leftFrameWidth", leftFrameWidth, getExpDate(180, 0, 0));
      }
    else if( is_nav4up )		// as onresize is not called on FF
    { leftFrameWidth = window.frames[0].innerWidth;
      //alert("Left offsetWidth: " + find_element("navi").offsetWidth
      //	+ "  innerWidht: " + window.frames[0].innerWidth);
      SetTopCookie("leftFrameWidth", leftFrameWidth, getExpDate(180, 0, 0));
    }
    leftFrameHidden = true;
    if( !oninit ) SetTopCookie("leftFrameHidden", "true", getExpDate(180, 0, 0));
    w = 0;
  }
  else
  { if( w == null )			// show
      if( ((w = leftFrameWidth) == null) || (w == 0) ) 
      { if( ((w = GetTopCookie("leftFrameWidth")) == null) || (w == 0) ) w = "33%";
        leftFrameWidth = w;
      }
    //if( is_nav4up ) alert("w: " + w);
    leftFrameHidden = false;
    if( !oninit ) SetTopCookie("leftFrameHidden", "false", getExpDate(180, 0, 0));
  }
  frameset.cols = w + ",*";
}

function ToggleLeftFrame( elem, hide )
{ var oninit = false;
  if( hide !== null ) leftFrameHidden = !hide, oninit = true;	// indicate restoring
  //if( hide !== undefined ) ...				// IE 5.0 has no undefined!
  if( leftFrameHidden )
  { if( elem ) elem.title = HIDE_LEFT_TITLE;
    ResizeLeftFrame(false, null, oninit);
  }
  else	// false or null
  { if( elem ) elem.title = SHOW_LEFT_TITLE;
    ResizeLeftFrame(true, null, oninit);
  }
  //top.status = elem.title;
}

function navitoggle_onclick( evt )
{ if( !evt ) evt = window.event;
  var elem = evt.target ? evt.target : evt.srcElement;		// element received the event
  if( elem.nodeType == 3 ) elem = elem.parentNode;		// text node -> A
  parent.ToggleLeftFrame(elem,null);
}

function frameset_onunload( evt )
 // Does not work in FF
{  if( leftFrameWidth != null )
     SetTopCookie("leftFrameWidth", leftFrameWidth, getExpDate(180, 0, 0));
}

function navi_onresize()
 // Seems not called on FF
{ parent.leftFrameWidth = is_nav4up ? window.innerWidth
			: parent.find_element("navi").offsetWidth;
	//(document.body.clientWidth + 16); //+ scrollbar
  //parent.status = "Left offsetWidth: " + parent.find_element("navi").offsetWidth
  //        + "  clientWidht: " + document.body.clientWidth;
}

function cont_onload( evt )
 // Called for every content page, i.e. that has cont_onopen in the HEAD.
{ // Check for the frameset's slot: if null, the frame layout has not been
  // restored or saved in this session yet.
  if( (window != parent) && (parent.leftFrameHidden === null) )
    if( leftFrameHidden = GetTopCookie("leftFrameHidden") )  // local var not affecting frameset's
      parent.ToggleLeftFrame(find_element("yh_navitoggle"), (leftFrameHidden == "true"));
}
// ]]-->