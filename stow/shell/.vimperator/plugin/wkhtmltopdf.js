let INFO = xml`
<plugin name="WkHTMLtoPDF" version="1.0.0"
summary="Create PDF from this HTML"
lang="en-US"
xmlns="http://vimperator.org/namespaces/liberator">
<author email="amkhlv@gmail.com">Andrei Mikhailov</author>
<license>New BSD License</license>
<project name="Vimperator" minVersion="2.4"/>
<p></p>
<item>
<tags>:wkhtmltopdf</tags>
<spec>:wkhtmltopdf <a>file</a></spec>
<description><p></p></description>
</item>
</plugin>
`;

(function () {
    commands.add(
        ['wkhtmltopdf'],
        'Create PDF from this HTML',
        function() {
            var fte = document.getElementById("urlbar").value.replace(/^file:\/\//,"").replace(/#.*/,"");
            var f1 = io.system("readlink -f " + fte).replace(/\s*$/,'');
            var pdffile = f1.replace(/\.html$/, '.pdf');
            var bname = io.system('basename ' + f1);
            var dirname = io.system('dirname ' + f1);
            var pdffilebasename = io.system('basename ' + pdffile);
            io.system("uxterm -e 'cd " + dirname + " ; wkhtmltopdf " + bname + " " + pdffilebasename + "; ls -lh " + pdffilebasename + "; tcsh '");
        },
        {
            literal: false    
        },
        true
    );
    })()


