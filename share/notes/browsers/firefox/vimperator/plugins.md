Minimal example
===============

      let INFO = xml`
      <plugin name="GiveAlert"
              version="1.0.0"
              summary="Give alert at start of Firefox and with command myalert"
              lang="en-US"
              xmlns="http://vimperator.org/namespaces/liberator">
      <author email="amkhlv@gmail.com">Andrei Mikhailov</author>
      <license>New BSD License</license>
      <project name="Vimperator" minVersion="2.4"/>
      <p></p>
      <item>
      <tags>:myalert</tags>
      <spec>:myalert></spec>
      <description><p></p></description>
      </item>
      </plugin>
      `;

      (function () {
          alert("Hi Andrei");
          commands.add(
              ['amscrbl'],
              'give some alert',
              function() {
                  alert("Here we are!");
              },
              {
                  literal: false   
              },
              true
          );
          })()

Format of Ex Command specification
==================================

See the source file `~/a/git/vimperator-labs/common/content/commands.js`



