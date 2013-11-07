var doc = null, editor = null;

function setDoc(docName) {
    document.title = "ShareJS+CodeMirror: " + docName;
    // don't set the standard one or we'll create two entries in the users history
    if(docName != "cm")
        location.hash = docName;

    sharejs.open(docName, 'text', function(error, newDoc) {
        if (doc !== null) {
            doc.close();
            doc.detach_cm();
        }

        doc = newDoc;

        if (error) {
            console.error(error);
            return;
        }
        doc.attach_cm(editor);
    });
};

function evalClojure(string) {
    $.post("/repl", string)
        .done(function(data) {
            $.each(jQuery.parseJSON(data), function (_, reply) {
                if (reply.value) {
                    $("#repl").append(reply.value + "\n");
                }
            });
        });
}

var editor;

window.onload = function() {
    editor = CodeMirror(document.body,
                        { mode: "clojure", tabSize: 2, extraKeys:
                          {
                              "Ctrl-Enter": function(cm) {
                                  evalClojure(cm.getSelection());
                              }
                          }
                        });
    var namefield = document.getElementById('namefield');

    var docName = location.hash.substr(1);
    if(docName.length < 1)
        docName = 'cm';
    setDoc(docName);  // Hooking ShareJS and CodeMirror for the first time.
    namefield.value = docName;

    function namefieldChanged() {
        var docName = namefield.value;
        if (docName) setDoc(docName);
    }

    if (namefield.addEventListener) {
        namefield.addEventListener('input', namefieldChanged, false);
    } else {
        namefield.attachEvent('oninput', namefieldChanged);
    }
};
