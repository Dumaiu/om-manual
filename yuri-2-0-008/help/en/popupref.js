<!--[CDATA[
/*** YstokHelp - Pop-up windows and menus
 *** Copyright (c) 2009-2012 Dr. Dmitriy Ivanov. All rights reserved.
 *** http://lisp.ystok.ru/yhelp/
 ***
 *** Must be linked after help.js
 ***/
/************************************** libDHTML **************************************/
var agt = navigator.userAgent.toLowerCase();
var is_major = parseInt(navigator.appVersion);

var is_nav  = (agt.indexOf('mozilla') >= 0) && (agt.indexOf('compatible') == -1)
	    && (agt.indexOf('spoofer') == -1)
var is_nav4	= is_nav && (is_major == 4);
var is_nav4up	= is_nav && (is_major >= 4);
var is_nav5up	= is_nav && (is_major >= 5);	// true for Firefox 2.0

var is_ie_ind = agt.indexOf("msie");
var is_ie     = (is_ie_ind >= 0)
	      ? (("56789".indexOf(agt.charAt(is_ie_ind+5)) >= 0) 
		  ? (agt.charCodeAt(is_ie_ind+5) - "0".charCodeAt(0)) : is_major)
	      : 0;
var is_ie4up  = (is_ie >= 4)
var is_iemac  = (agt.indexOf("mac") != -1) ? is_ie : 0;
 
//alert("appName: "+navigator.appName+" appVersion: "+navigator.appVersion+" userAgent: "+agt + " is_ie " + is_ie);
var is_js_ext = !((is_ie && is_major<4) || (is_nav && is_major<3));

var is_ie6css;
function init_libDHTML()
{ is_ie6css = (document.compatMode && document.compatMode.indexOf("CSS1") >= 0) ?  true : false; }

add_onload_event( init_libDHTML );  		// can be overriden by body.onload

function style_or_element( obj )
 // Convert object name string or object reference into a valid style or NN4 layer reference
{   var elem = find_element(obj);
    return (elem && elem.style) ? elem.style : elem; //isCSS
}

function get_width( obj )			// Retrieve the rendered width of an element
{ var elem = find_element(obj);
  var result = elem.offsetWidth ? elem.offsetWidth
	     : (elem.clip && elem.clip.width) ?  elem.clip.width
	     : (elem.style && elem.style.pixelWidth) ? elem.style.pixelWidth
	     : 0;
  return parseInt(result);
}
   
function get_height( obj )			// Retrieve the rendered height of an element
{ var elem = find_element(obj);
  var result = elem.offsetHeight ? elem.offsetHeight
	     : (elem.clip && elem.clip.height) ? elem.clip.height
	     : (elem.style && elem.style.pixelHeight) ? elem.style.pixelHeight
	     : 0;
  return parseInt(result);
}
   
function get_client_area_width()		// Content width of the browser window
{ return window.innerWidth ? window.innerWidth 
         : is_ie6css ? document.body.parentElement.clientWidth   // HTML element's clientWidth
	 : (document.body && document.body.clientWidth) ? document.body.clientWidth
	 : 0;
}
   
function get_client_area_height()		// Content height of the browser window
{ return window.innerHeight ? window.innerHeight
	: is_ie6css ? document.body.parentElement.clientHeight	// HTML element's clientHight
        : (document.body && document.body.clientHeight) ? document.body.clientHeight
        : 0;
}

function get_element_coords( obj )
 // Determine position of a nonpositioned element.
 // For possitioned via CSS, use style.left è style.top.
{ var x = 0, y = 0;
  for( var elem = find_element(obj); elem; elem = elem.offsetParent )   // For IE 6 CSS compatibility
     x += elem.offsetLeft, y += elem.offsetTop;				// only, walk through parents
  if( is_iemac && (typeof document.body.leftMargin != "undefined") )	// IE Mac bug
    x += document.body.leftMargin, y += document.body.topMargin;
  return {left:x, top:y};
}

function get_event_coords( evt )
 // Obtain coordinates relative to the space occupied by the entire page,
{ var x = 0, y = 0;
  if( evt.pageX ) x = evt.pageX, y = evt.pageY;
  else if( evt.offsetX || evt.offsetY ) x = evt.offsetX, y = evt.offsetY; // is it really relative?
  else if( evt.clientX )	// w.r.t client area of the window excluding decorations and scrollbars
  { x = evt.clientX; // + document.body.scrollLeft - document.body.clientLeft;
    y = evt.clientY; //+ document.body.scrollTop - document.body.clientTop;
    /*var eltHTML = document.body.parentElement;		// include HTML element space,
    if( eltHTML && eltHTML.clientLeft )				// if applicable
      x += eltHTML.scrollLeft - eltHTML.clientLeft,
      y += eltHTML.scrollTop - eltHTML.clientTop;*/
  }
  return {left:x, top:y};
}

function escape_quote( arg )
 // Convert ECMAScript string to sequence (also used as an attribute value)
{ var s = "", ch;
  for( var len = arg.length, i = 0; i < len; i++ )
    switch (ch = arg.charAt(i))
    { case "\'": s = s + "&#039;"; break;
      case "\\": s = s + "&#092;"; break;
      default: s = s + ch;
    }
  return s;
}
/********************************** End of libDHTML ************************************/

var svVisible, svHidden;			// values of style.visibility or element itself
if( is_nav4 ) svVisible = "show", svHidden = "hide";
else svVisible = "visible", svHidden = "hidden";

// Globals
var POPUP_DIV_ID = "yh_popup_div";
var POPUP_FRAME_ID = "yh_popup_frame";
var POPUP_MENU_DIV_ID = "yh_popup_menu";
var BLANK_PAGE_URI = "about:blank";

// Parametes for calculating the size of popup windows
var client_w = 640, client_h = 480;
var scrollbar_width = 16;
var scrollbar_d = 10;
var GRATIO = 0.618;				// golden ratio height/width
var MAX_PCT_OF_CLIENT_W	= 0.8, MAX_PCT_OF_CLIENT_H = 0.8;
var max_allowed_dx = 3, max_allowed_dy	= 3;

var popupDiv = null;
var popupDivStyle = null;
var popupFrame = null;
var popupFrameStyle = null;
var popupClickX = 0, popupClickY = 0;		// coords relative to parent page adjusted by handler

var popupMenuDiv = null;
var popupMenuDivStyle = null;

/** Q: Do we need these?
var popupTimeoutExpired = false;
var popupMenuTimeoutExpired = false;		// prevent spurious clicks until gets true
function IsPopup()				// for popup menus inside a popup?
{ return (window != parent) && (window.name == POPUP_FRAME_ID); }
**/

/** The following two should be called at the very bottom of BODY.
 **/
function make_popup()
{ if( popupDiv == null ) // && is_dhtml_supported )
  { document.write("<div id='" + POPUP_DIV_ID + "'>");
    document.write("<iframe id='" + POPUP_FRAME_ID + "' name='" + POPUP_FRAME_ID
		   + "' src='" + BLANK_PAGE_URI + "' frameborder='0'></iframe>");
    document.writeln("</div>");
} }
function make_popup_menu()
{ document.writeln("<div id='" + POPUP_MENU_DIV_ID + "' onkeyup='popup_onkeyup()'></div>");
}

function EnsurePopupDiv()
{ popupDiv = find_element(POPUP_DIV_ID);
  popupFrame = window.frames[POPUP_FRAME_ID];
  if( is_nav4 ) 		// style works nowhere
  { popupDivStyle = popupDiv;
    popupFrameStyle = find_element(POPUP_FRAME_ID);
  }
  else 				//popupFrameStyle = style_or_element(POPUP_FRAME_ID);
  { popupDivStyle = popupDiv.style;
    popupFrameStyle = find_element(POPUP_FRAME_ID).style;
} }

function EnsurePopupMenuDiv()
{ popupMenuDiv = find_element(POPUP_MENU_DIV_ID);
  popupMenuDivStyle = style_or_element(popupMenuDiv);
}

/** IFRAME helpers: elem - An instance of IFRAME
 ** IE: scrollWidth/Height - properties for the needed space
 **/
function document_body( elem )
{ return elem.contentDocument ? elem.contentDocument.body	// W3C
	: elem.contentWindow ? elem.contentWindow.document.body	// IE
	: elem.document.body;
}

function natural_width( elem )
{ var body = document_body( elem );
  return (body==null) ? 400
	: body.scrollWidth ? body.scrollWidth			// IE only
	: body.offsetWidth;
}

function natural_height( elem )
{ var body = document_body( elem );
  return (body==null) ? 300
	: body.scrollHeight ? body.scrollHeight			// IE only
	: body.offsetHeight;
}

function ComputeNaturalSize( elem )
/** Value: {width,height} structure.
 ** ASSUMPTION: Globals client_w and client_h has set.
 ** NB:	Play against width in order to make the frame wide enough to fit content.
 **	Start with large, then decrease and wait for the moment when scrollHeight changes,
 **	i.e. it appears. In order to hide it, rollback the widht to the previous value.
 **/
{ if( is_iemac ) return {width:300, height:400};
  var max_w = client_w * MAX_PCT_OF_CLIENT_W, max_h = client_h * MAX_PCT_OF_CLIENT_H;
  var ratio = client_h / client_w;
  var wanted_w = max_w, wanted_h = max_h;
  if( ratio > GRATIO ) wanted_h = max_w * GRATIO; else wanted_w = max_h / GRATIO;
  var dx, dy;
  var w = wanted_w, h; 					// start from client width according to GRATIO
  var body = document_body(elem);
  elem.resizeTo(50, 50);				// resizing twice forces re-rendering document
  elem.resizeTo(w, natural_height(elem) + scrollbar_width);
		
  var natural_h = natural_height(elem) + scrollbar_width;
  if( natural_h > wanted_h )					// does not fit golden height wanted
  { elem.resizeTo(max_w , natural_h);				// try increasing width to max possible
    natural_h = natural_height(elem) + scrollbar_width;
    if( natural_h > max_h )					// did not help - must have scrollbar
    { natural_h = max_h;
      w = max_w, h = max_h;
    }
    else 							// popup still can fit the client area
    { h = natural_h;						// choose the same h/w rate as the area
      w = max_w;
      dx = -max_w / 2;						// downsize the delta from max_w
      while( true )
      { w = w + dx;
	elem.resizeTo(w, natural_h);
	dy = natural_height(elem) + scrollbar_width - w * ratio;
	if( max_allowed_dy < dy ) dx = Math.abs(dx) / 2;	// higher than wanted - need wider
	else if( dy < -max_allowed_dy ) dx = -Math.abs(dx) / 2;	// shorter than wanted - need narrower
	else break; 						// height is close enough to wanted
	if( Math.abs(dx) < max_allowed_dx ) break;		// next step is too miserable
      }
      w = natural_width(elem);					//+ scrollbar_width;
      h = natural_height(elem);					//+ scrollbar_width;	
      body.scroll = "no";					// do not show scroll bar any longer
      //if( h < 100 ) h = 100;					// unexlainable IE flakes
  } }
  else								// fits golden wanted_h - make shorter
  { dx = -wanted_w / 2;						// downsize the delta from wanted_w
    while( true )
    { w = w + dx; 
      elem.resizeTo(w, natural_h);
      dy = natural_height(elem) + scrollbar_width - w * GRATIO;
      if( max_allowed_dy < dy ) dx = Math.abs(dx) / 2;		// higher than wanted - need wider
      else if( dy < -max_allowed_dy ) dx = -Math.abs(dx) / 2;	// shorter than wanted - need narrower
      else break;			 			// height is close enough to wanted
      if( Math.abs(dx) < max_allowed_dx ) break;		// next step is too miserable
    }
    w = natural_width(elem);  					//+ scrollbar_width;
    h = natural_height(elem);		 			//+ scrollbar_width;	
    body.scroll = "no"; 					// do not show scrollbar any longer 
  }
  if( is_ie == 4 ) w += scrollbar_width; 			// reserve a width for scrollbar IE4
  elem.resizeTo(w, h);
  if( body.scroll == "yes" ) elem.resizeTo(w, h += scrollbar_d); // if scrollbar visible
  if( body.scroll == "yes" ) elem.resizeTo(w += scrollbar_d, h); // increase a bit
  //if((dx = find_effective_style(elem,"padding-bottom","paddingBottom")) )    // does not work IE55!
  //&& (dy = parseInt(dx)) ) h += parseInt(dy);
  h += 11;					// work around all browser collapsing bottom space
  return {width:w, height:h};
}

function ComputePosition( click_x, click_y, w, h )
 // Args: click_x, click_y  Relative to page
 // Value: Structure {left,top}.
{ var x =  client_w + document.body.scrollLeft - w;
  if( click_x < x ) x = click_x;
  var y = client_h + document.body.scrollTop - h - 20; 
  if( click_y < y ) y = click_y;
  if( x < document.body.scrollLeft ) x = document.body.scrollLeft + 1;
  if( y < document.body.scrollTop ) y = document.body.scrollTop + 1;
  return {left:x,top:y};
}

function ResizeMovePopup()
 // First, compute size, then position, finally, resize and reposition both DIV and IFRAME
{ popupDivStyle.visibility = svHidden;
  client_w = get_client_area_width(), client_h = get_client_area_height();
  var size = ComputeNaturalSize(popupFrame);
  var w = size.width, h = size.height;
  var coords = ComputePosition(popupClickX, popupClickY, w, h);
  // IE assumes size to be external, so add 2 (DIV border) to 1+1 (IFRAME border)
  var delta = is_ie ? 4 : 2;
  popupDivStyle.width = w + delta, popupDivStyle.height = h + delta;
  popupFrameStyle.width = w, popupFrameStyle.height = h;	// without resizing FF feels bad
  popupDivStyle.left = coords.left, popupDivStyle.top = coords.top;
  popupDivStyle.visibility = svVisible;
  return false;
}

function PopupTimeout()
{ if( popupFrame.loaded ) popup_onload_callback();
  else setTimeout("PopupTimeout()", 100);	 // wait until the popup frame finish loading
}

function popup_onload_callback()
{ ResizeMovePopup();
  //popupTimeoutExpired = true;
  popupFrame.focus();
  add_event(document,"click",HidePopups);	// no capture - should propagate to IFRAME!
}

function HidePopups()
 // Used as: popup_parent_onmousedown
{ //if( document.releaseCapture ) document.releaseCapture();	// turn off IE mouse event capture
  remove_event(document,"click",HidePopups);
  if( popupDiv && (popupDivStyle.visibility == svVisible) ) 
  { popupDivStyle.visibility = svHidden;
    popupFrame.location.replace(BLANK_PAGE_URI);
  }
  else if( popupMenuDiv && (popupMenuDivStyle.visibility == svVisible) )
  { popupMenuDivStyle.visibility = svHidden;
  }
  return true;
}

function popup_onkeyup( evt )
 // Dismiss popup on pressing Escape.
 // Firefox NB: Works in popupFrame but not in popupMenuDiv.
{ if( evt || (evt = window.event) ) 
    if( evt.keyCode == 27 )
    { HidePopups();
      if( evt.cancelBubble ) evt.cancelBubble = true;
      if( evt.stopPropagation ) evt.stopPropagation();
      return false;
    }
  return true;
}

/** Handlers to use inside <A onclick='popup(event)' onkeyup='' onkeydown=''> for invoking a popup
 ** NB: Navigator/Firefox seem to fire click after keyup.
 **/
var popupKeyCode = 0;		 	// saved code as nor keyCode nor button is defined for click

function popup_okd( evt ) { if( evt ? evt : (evt = window.event) ) popupKeyCode = evt.keyCode; }
function popup_oku( evt ) { if( !(is_nav && (popupKeyCode == 13)) ) popupKeyCode = 0; }

function popup( evt )
{ HidePopups();
  if( evt ? evt : (evt = window.event) )
  { var coords = get_event_coords(evt);
    var elem = (evt.target) ? evt.target : evt.srcElement;
    var height = get_height(elem);
    if( popupKeyCode == 13 || coords.left == 0 ) //|| popupClickX == 0 )	// everyting =0 for FF
    { coords = get_element_coords(elem);
      popupClickX = coords.left, popupClickY = coords.top + height;
    }
    else popupClickX = coords.left - 8,
	 popupClickY = coords.top + height/2;			// usually clicks at the middle
    popupKeyCode = 0;
    if( elem.nodeType == 3 ) elem = elem.parentNode;		// text node -> A
    //alert( "coords=" + popupClickX + "," + popupClickY); 
    if( popupDiv == null ) EnsurePopupDiv();
    //popupTimeoutExpired = false;
    popupFrame.location.href = elem.href;
    setTimeout("PopupTimeout()", (is_iemac ? 300 : 100));
    if( evt.preventDefault ) evt.preventDefault();		// no follow href 
    evt.returnValue = false;
    return false;
} }

/*** Popup Menu as Layer
 *** mi ::= {uri,title,description}
 ***/
function PopupMenuInnerHTML( title, items )
 // Args: items ::= [uri1, title1, description1, uri2, title2, description2,...] or [mi1, mi2,...]
 //	  Combinations alse allowed:
 //		['uri1.html','Title 1','Description 1',{uri:'uri2.html',title:'Title 2'}]
{ var uri, description;
  var s =  title ? "<p>" + escape_quote(title) + "</p>" : "";
  for( var len = items.length, i = 0; i < len; )
  { if( typeof(uri = items[i]) == "string" )
    { title = items[++i];
      if( ++i < len ) description = items[i++]; else description = null;
    }
    else title = uri.title, description = uri.description, uri = uri.uri, i++;
    s += "<a href='" + uri + (description ? "' title='" + escape_quote(description) : "") + "'>"
					//+ "' onkeyup='popup_onkeyup()" - does not help on FF
      + escape_quote(title) + "</a>";
  }
  if( is_iemac ) s += "<div></div>";			// IE5 Mac hack: it needs a block after
  return s;
}

function PopupMenuTimeout()
{ //popupMenuTimeoutExpired = true;
  add_event(document,"click",HidePopups);		// no capture - let propagate to IFRAME!
  var coll = popupMenuDiv.getElementsByTagName("A");
  if( coll && (coll.length > 0) ) coll[0].focus();
}

/** Handler to use inside <A onclick='popup_menu(event)'> for invoking a popup menu
 **/
function popup_menu( evt, title, items )
 // Args: evt [title] items-array
 // CAUTION: setCapture does not fit as we exploit HREFs in built-in way.
{ HidePopups();    					// hide any existing menu just in case
  if( evt ? evt : (evt = window.event) )
  { if( popupMenuDiv == null ) EnsurePopupMenuDiv();
    var coords = get_event_coords(evt);
    if( popupKeyCode == 13 ) //|| popupClickX == 0 )	// everyting =0 for FF
    { var elem = (evt.target) ? evt.target : evt.srcElement;
      coords = get_element_coords(elem);
      popupClickX = coords.left, popupClickY = coords.top + get_height(elem);
    }
    else popupClickX = coords.left - 8, popupClickY = coords.top;
    popupKeyCode = 0;
    client_w = get_client_area_width(), client_h = get_client_area_height();
    if( typeof title != "string" ) items = title, title = null;		// title omitted
    var s = PopupMenuInnerHTML(title,items);
    if( is_nav4 ) { var doc = popupMenuDiv.document; doc.write(s); doc.close(); }
    else popupMenuDiv.innerHTML = s;
    var coords = ComputePosition(popupClickX, popupClickY,
				 get_width(popupMenuDiv), get_height(popupMenuDiv));
    popupMenuDivStyle.left = coords.left, popupMenuDivStyle.top = coords.top; //+ "px"
    popupMenuDivStyle.visibility = svVisible;
    //popupMenuTimeoutExpired = false;
    setTimeout("PopupMenuTimeout()", 100);
    if( evt.preventDefault ) evt.preventDefault();
    evt.returnValue = false;
    return false;
} }
// ]]-->