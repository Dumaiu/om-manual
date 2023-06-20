<!--[CDATA[
/*** YstokHelp - Pop-up windows
 *** Copyright (c) 2009-2012 Dr. Dmitriy Ivanov. All rights reserved.
 *** http://lisp.ystok.ru/yhelp/
 ***
 *** Small script for linking from pure pop-up pages instead of help.js and popup.js
 *** Must in HEAD: <BASE target="_parent">
 ***/
var loaded = false;			// checked by the parent in timeout cicle

/******************************** libDHTML **********************************/
function add_event( elem, evtType, fun, capture )
{ if( elem.addEventListener )
    elem.addEventListener(evtType, fun, (capture || false));
  else if( elem.attachEvent ) 
    elem.attachEvent("on" + evtType, fun);
  else elem["on" + evtType] = fun;	// for IE/Mac, Nav4, and older
}

function add_onload_event( fun )
{ if( window.addEventListener || window.attachEvent ) 
    add_event(window,"load", fun, false);
  else
  { var old = window.onload; 		// make closure over the old onload handler
    if( old ) window.onload = function() { old(); fun(); };
    else window.onload = fun;
} }
/***************************** End of libDHTML *******************************/

function popup_onopen()
{ add_onload_event(popup_onload);
}

function popup_onload()
 // DO NOT TRY to call parent.popup_onload_callback from here!
 // That would leads to rendering the popup frame on return to the content page via history.
{ /*for( var coll = document.links, i = 0; i < coll.length; i++ ) 
    if( (coll[i].href != null) && (coll[i].target != "_self") ) coll[i].target = "_parent";*/
  add_event(document, "click", popup_onclick);
  add_event(document, "keyup", popup_onkeyup, true);
  loaded = true;
}

function popup_onclick( evt )
 // Handle click anywhere inside IFRAME
 // NB: If the TARGET attribute is not specified for an A element, property elem.target returns:
 //	IE => null,
 //     FF => value inherited from BASE
{ //if( !popupTimeoutExpired || !popupMenuTimeoutExpired ) return false;
  if( evt || (evt = event) )
  { var elem = evt.target ? evt.target : evt.srcElement;
    if( elem.nodeType == 3 ) elem = elem.parentNode;
    if( elem && (elem.tagName == "A") )
    { //alert( "elem.target=" + elem.target );
      if( elem.target && (elem.target.toLowerCase() == "_self") ) ;
      else if( parent.HidePopups ) parent.HidePopups();
      return true;					// follow href
    }
    else
    { if( parent.HidePopups ) parent.HidePopups();
      if( evt.preventDefault ) evt.preventDefault();	// no follow href 
      evt.returnValue = false;
      return false;
} } }

function popup_onkeyup( evt )
 // Dismiss popup on pressing Escape
{ if( evt || (evt = window.event) ) 
    if( evt.keyCode == 27 )
    { if( parent.HidePopups ) parent.HidePopups();
      if( evt.cancelBubble ) evt.cancelBubble = true;
      if( evt.stopPropagation ) evt.stopPropagation();
      return false;
    }
  return true;
}

/** Calling another popup from this: just substitute target and let default onclick handler
 **/
function popup_okd( evt ) { return true; }	// no positioning needed
function popup_oku( evt ) { return true; }

function popup( evt )
{ //parent.window.popupTimeoutExpired = false;
  if( evt || (evt = event) )
  { var elem = evt.target ? evt.target : evt.srcElement;
    if( elem.nodeType == 3 ) elem = elem.parentNode;
    if( elem && (elem.tagName == "A") ) elem.target = "_self";
  }
  return true;
}

function popup_menu() { return true; }		// no menus inside popup
// ]]-->