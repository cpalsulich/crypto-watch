function deleteChoice(currency, address) {
    const url = "http://localhost:3000/";
    const xhr = new XMLHttpRequest();
    xhr.open("DELETE", url, true);
    xhr.setRequestHeader('Content-type','application/json; charset=utf-8');
    xhr.send(JSON.stringify({name: currency, address: address}));
    window.location.reload();
}
