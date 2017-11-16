importScripts(
  "sha512.js"
, "sha512crypt.js"
)

onmessage = function(e) {
  if (!e.isTrusted) {
    return
  }

  console.log('worker received message')
  console.debug(e)

  var msg = JSON.parse(e.data)
  console.debug(msg)

  var {tag, body} = msg

  switch (tag) {
    case "sha512crypt":
      var hash = sha512crypt(body.pass, body.salt)
      post('hash', {hash})
  }
}

function post(tag, body) {
  postMessage(JSON.stringify({tag, body}))
}
