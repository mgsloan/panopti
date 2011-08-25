
var t;
var waiting = 0;

function get(params, targetHtml) {
//    alert('get: ' + params);
    var http = false;
    if (window.XMLHttpRequest) { // Mozilla, Safari,...
        http = new XMLHttpRequest();
        if (http.overrideMimeType) {
            http.overrideMimeType('text/html');
        }
    } else {
        if (window.ActiveXObject) { // IE
            try {
                http = new ActiveXObject("Msxml2.XMLHTTP");
            } catch (e) {
                try {
                    http = new ActiveXObject("Microsoft.XMLHTTP");
                } catch (e) {}
            }
        }
    }
    if (!http) {
        targetHtml.innerHTML = '<span class="error">Kliens hiba.</span>';
    } else {

//        alert('get ready: x ' );

        if (waiting == 0) {
            t = setTimeout("drawWarning(1);", 1000);
        };
        waiting = waiting + 1;

        http.onreadystatechange = function () {
            if (http.readyState == 4) {
                targetHtml.innerHTML = http.responseText;
                waiting = waiting - 1;
                if (waiting == 0) {
                    clearTimeout(t);
                    setTimeout("clearWarning();", 500);
                };
            }
        }

        http.open('POST', "exercise/", true);
        http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        http.setRequestHeader("Content-length", params.length);
        http.setRequestHeader("Connection", "close");
        http.send(params);
    }
}

function drawWarning(n) {
    var r="Várakozás."; 
    for (var a=0; a< n % 3; a++) r+='.';
    document.getElementById('info').style.display = "block";
    document.getElementById('info').innerHTML = '<div class="wait">' + r + '</div>';
    t = setTimeout('drawWarning(' + (n+1) + ');', 500);
}

function clearWarning() {
    document.getElementById('info').style.display = "none";
    document.getElementById('info').innerHTML = "";
}

function getPos(evt, obj) { //this is the main function
    var img_x;
    var img_y;
    var c;
	if (document.all) { // MSIE
		img_x = evt.offsetX;
		img_y = evt.offsetY;
	} else { // Netscape, etc.
		img_x = evt.pageX;
		img_y = evt.pageY;

		c = findPos(obj); // here I get the images position
		img_x -= c[0];
		img_y -= c[1];

//		c = checkscroll(); // then I check the scrollbar
//		img_x += c[0];
//		img_y += c[1];
	}
	return [img_x,img_y];
}

function findPos(obj) {
	var curleft = curtop = 0;
	if (obj.offsetParent) {
		curleft = obj.offsetLeft;
		curtop = obj.offsetTop;
		while (obj = obj.offsetParent) {
			curleft += obj.offsetLeft;
			curtop += obj.offsetTop;
		}
	}
	return [curleft,curtop];
}

// még nem jó..
function findScrolled(obj) {
	var curleft = curtop = 0;
	if (obj) {
		curleft = obj.scrollLeft;
		curtop = obj.scrollTop;
		while (obj.tagName.toLowerCase () != "html") {
            obj = obj.parentNode;
			curleft += obj.scrollLeft;
			curtop += obj.scrollTop;
		}
	}
	return [curleft,curtop];
}

function findDiff(obj) {
    var c = findPos(obj);
    var d = findScrolled(obj);
    return [c[0]-d[0],c[1]-d[1]];
}

function yTop() {
	if(window.pageYOffset) {
		return window.pageYOffset;
	} else if(document.body.scrollTop) {
		return document.body.scrollTop;
	}
}

/*
function checkscroll() {
    var x;
    var y;
	if(window.pageXOffset) {
		x = window.pageXOffset;
	} else if(document.body.scrollLeft) {
		x = document.body.scrollLeft;
	}
	if(window.pageYOffset) {
		y = window.pageYOffset;
	} else if(document.body.scrollTop) {
		y = document.body.scrollTop;
	}

	if(x == undefined){
		x = 0;
	}
	if(y == undefined){
		y = 0;
	}
	return [x,y];
}
*/
function getData(obj) {
    return encodeURIComponent(document.getElementById('tarea' + obj).value);
}

function target(obj) {
    return document.getElementById('res' + obj);
}

function getOne(a, t, x) {
    get(a + "&lang=hu&x=" + getData(x) + "&y=", target(t));
}

function getTwo(a, t, x, y) {
    get(a + "&lang=hu&x=" + getData(x) + "&y=" + getData(y), target(t));
}

function resetForms() {
    var forms = document.getElementsByTagName("form");
    for(var index=0;index<forms.length;index++) {
        if (forms[index].className == 'resetinterpreter') { 
            forms[index].reset(); 
        }
    }
}
/*
var Black = "black";
var White = "white";
var Gray = "gray";
var Blue = "blue";
var Red = "red";
var Yellow = "yellow";
var Green = "green";
*/
function set(a,b,c) {
    document.getElementById(a).setAttribute(b,c);
}

function replace(a,b) {
    var q= document.getElementById(a);
    q.replaceChild(document.getElementById(b).cloneNode(true), q.firstChild);
}

// after w3c slidy, http://www.w3.org/Talks/Tools/Slidy2/

var slidy = {
  key_wanted: false,
  slide_number: 0, // integer slide count: 0, 1, 2, ...
  view_all: true,  // true: view all slides + handouts

  show_slide: function () {
    window.scrollTo(0,0);
    set_class(document.getElementsByTagName('body')[0].children[slidy.slide_number], "current");
  },

  switch_slide: function (event, n) {
    if (slidy.view_all) {
      return true;
    } else {
      var x = document.getElementsByTagName('body')[0].children;
      if (n >= 0 && n < x.length && n != slidy.slide_number) {
        set_class(x[slidy.slide_number], "");
        slidy.slide_number = n;
        slidy.show_slide();
      }
      return slidy.cancel(event);
    }
  },

  // needed for Opera to inhibit default behavior
  // since Opera delivers keyPress even if keyDown
  // was cancelled
  key_press: function (event) {
    if (!event)
      event = window.event;

    if (!slidy.key_wanted)
      return slidy.cancel(event);

    return true;
  },

  //  See e.g. http://www.quirksmode.org/js/events/keys.html for keycodes
  key_down: function (event) {
    var key, target, tag;

    slidy.key_wanted = true;

    if (!event)
      event = window.event;

    // kludge around NS/IE differences 
    if (window.event)
    {
      key = window.event.keyCode;
      target = window.event.srcElement;
    }
    else if (event.which)
    {
      key = event.which;
      target = event.target;
    }
    else
      return true; // Yikes! unknown browser

    // ignore event if key value is zero
    // as for alt on Opera and Konqueror
    if (!key)
       return true;

    if (slidy.special_element(target))
      return true;

    // check for concurrent control/command/alt key
    // but are these only present on mouse events?
    if (event.ctrlKey || event.altKey || event.metaKey)
       return true;

    if (key == 32 || key == 39) { // space bar, right arrow
      slidy.switch_slide(event, slidy.slide_number + 1);
    } else if (key == 37) { // left arrow
      slidy.switch_slide(event, slidy.slide_number - 1);
    } else if (key == 36) { // Home
      slidy.switch_slide(event, 0);
    } else if (key == 35) { // End
      slidy.switch_slide(event, document.getElementsByTagName('body')[0].children.length - 1);
    } else if (key == 65 && document.getElementsByTagName('body')[0].children.length > 3) {  // A

        var x = document.getElementsByTagName('body')[0].children;

        if (slidy.view_all) {
          set_class(document.getElementsByTagName('body')[0], "single_slide");
          // var y = 0; //yTop();
          // slidy.slide_number = 0;
          // while (findDiff(x[slidy.slide_number])[1] < 0
          //       && slidy.slide_number < x.length - 1) {
          //  slidy.slide_number += 1;
          // }
          slidy.show_slide();
        } else {
          set_class(document.getElementsByTagName('body')[0], "");
          x[slidy.slide_number].scrollIntoView();
          // set_class(x[slidy.slide_number], "");
        }
        slidy.view_all = !slidy.view_all;
        return slidy.cancel(event);
    }
    return true;
  },

  special_element: function (e) {
    var tag = e.nodeName.toLowerCase();

    return e.onkeydown ||
      e.onclick ||
      tag == "a" ||
      tag == "embed" ||
      tag == "object" ||
      tag == "video" ||
      tag == "audio" ||
      tag == "input" ||
      tag == "textarea" ||
      tag == "select" ||
      tag == "option";
  },

  cancel: function (event) {
    if (event)
    {
       event.cancel = true;
       event.returnValue = false;

      if (event.preventDefault)
        event.preventDefault();
    }

    slidy.key_wanted = false;
    return false;
  },
};

function set_class(element, name) {
  if (typeof element.className != 'undefined') {
    element.className = name;
  } else {
    element.setAttribute("class", name);
  }
};

function add_listener(event, handler) {
    if (window.addEventListener)
      document.addEventListener(event, handler, false);
    else
      document.attachEvent("on"+event, handler);
};

// attach event listeners for initialization
function slidy_init() { 
  add_listener("keydown", slidy.key_down);
  add_listener("keypress", slidy.key_press);
};



