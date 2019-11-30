from django.shortcuts import render
from django.template import loader
from django.http import HttpResponse
from django.conf import settings
from django.contrib.auth.decorators import login_required

def loginHandler(request):
    template = loader.get_template('login.html')
    context = { "root" : settings.AROOT }
    return HttpResponse(template.render(context, request))

@login_required
def redirectHandler(request,path=""):
    if request.user.is_authenticated:
        response = HttpResponse()
        rnm = request.user.username
        nm = rnm if rnm.strip() else 'error'
        response['X-Accel-Redirect'] = '/redirector/' + nm + '/' + path
        if (path[-4:] == '.css') : response['Content-Type'] = 'text/css; charset=utf-8'
        if (path[-4:] == '.pdf') : response['Content-Type'] = 'application/pdf'
        return response
    else :
        template = loader.get_template('login.html')
        context = dict()
        return HttpResponse(template.render(context, request))

