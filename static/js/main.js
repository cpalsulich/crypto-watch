function deleteChoice(currency, address) {
    const url = "/";
    const xhr = new XMLHttpRequest();
    xhr.open("DELETE", url, true);
    xhr.setRequestHeader('Content-type','application/json; charset=utf-8');
    xhr.send(JSON.stringify({name: currency, address: address}));
    window.location.reload();
}

function refreshSession() {
    const xhr = new XMLHttpRequest();
    xhr.open("POST", "/refreshSession", true);
    xhr.send();
}

window.onload = refreshSession;
