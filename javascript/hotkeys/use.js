hotkeys('ctrl+a,ctrl+b,r,f', function(event,handler) {
    switch(handler.key){
    case "ctrl+a":alert('you pressed ctrl+a!');break;
    case "ctrl+b":alert('you pressed ctrl+b!');break;
    case "r":alert('you pressed r!');break;
    case "f":alert('you pressed f!');break;
    }
});

hotkeys('ctrl+a', {
    element: document.getElementById('area')
}, function(event,handler) {
    console.log('here', event, handler)
    event.preventDefault();
    event.target.value = 'a';
});
