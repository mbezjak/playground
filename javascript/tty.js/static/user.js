(function() {
    var sequence = {
        esc : '\x1b'
    };
    var openWindow = function(interactWithTerminal) {
        tty.once('open window', function(win) {
            var term = win.tabs[0];
            interactWithTerminal(term);
        });
        new tty.Window;
    };
    var interact = function(term) {
        var send   = function(text) { term.send(text); };
        var sendLn = function(text) { send(text + '\n'); };

        sendLn('cd /tmp');
        sendLn('rm -f test.txt');
        sendLn('vim test.txt');
        send('i');
        send('contents of a test file');
        send(sequence.esc);
        sendLn(':wq');

        setTimeout(function() {
            sendLn('pwd');
            sendLn('cat test.txt');
        }, 1000);
    };
    var openAndInteract = function() {
        openWindow(interact);
    };


    tty.once('open', function() {
        setTimeout(openAndInteract, 1000);
    });

})();
