let INFO = xml`
<plugin name="OpenScribbleFile" version="1.0.0"
summary="Open scribble files with emacsclient."
lang="en-US"
xmlns="http://vimperator.org/namespaces/liberator">
<author email="amkhlv@gmail.com">Andrei Mikhailov</author>
<license>New BSD License</license>
<project name="Vimperator" minVersion="2.4"/>
<p></p>
<item>
<tags>:scribble</tags>
<spec>:scribble <a>file</a></spec>
<description><p></p></description>
</item>
</plugin>
`;

(function () {
    commands.add(
        ['scribble'],
        'Open Scribble File',
        function() {
            var fte = document.getElementById("urlbar").value.replace(/^file:\/\//,"").replace(/#.*/,"");
            var f1 = io.system("readlink -f " + fte).replace(/\s*$/,'');
            var filetoedit = null;
            if (fte.search(/\/index\.html$/) >= 0) {
                filetoedit = f1.replace(/\/index\.html$/, '.scrbl');
            } else {
                var f2 = f1.replace(/\.html$/, '.scrbl');
                var bname = io.system('basename ' + f1);
                var dirname = io.system('dirname ' + f1);
                var scrbl = bname.replace(/\.html$/, '.scrbl');
                var f3 = io.system('[ -e "' + f2 + '" ] ' +
                                   ' && { echo "' + f2 + '" ; } ' +
                                   ' || { cd "' + dirname + '"; cd .. ; echo "$(pwd)/' + scrbl +'" ; }');
                filetoedit = io.system('[ -e "' + f3 + '" ] ' +
                                       ' && { echo "' + f3 + '" ; } ' +
                                       ' || { echo "$(dirname "' + dirname + '")/$(basename "' + dirname + '").scrbl" ; }');
            }
            if (filetoedit) {
                alert("found scribble: " + filetoedit);
                io.system("emacsclient " + filetoedit + " & ");
            } else {
                alert("ERROR: scribble not found!");
            }
        },
        {
            literal: false    
        },
        true
    );
    })()
