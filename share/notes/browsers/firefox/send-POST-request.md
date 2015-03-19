POST request
============

Taken from [stackoverflow](http://stackoverflow.com/questions/1230140/how-do-i-post-to-a-web-page-using-firebug):

Open console by pressing `Ctrl-Shift-k` and type:

    var formPost = document.createElement('form'); formPost.method = 'POST'; formPost.action = 'https://www.google.com'; document.body.appendChild(formPost); formPost.submit();

Make sure to replace `https://www.google.com` with whatever you need.
