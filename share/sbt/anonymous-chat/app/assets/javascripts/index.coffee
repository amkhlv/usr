$ ->
  ws = new WebSocket $("body").data("ws-url")
  tok = $("body").data("tok")
  console.log("TOK= " + tok)
  ws.onopen = (event) ->
    ws.send(JSON.stringify({token: tok}))
  ws.onmessage = (event) ->
    message = JSON.parse event.data
    switch message.type
      when "message"
        $("#board tbody").append("<tr><td>" + message.uid + "</td><td bgcolor=\"" + message.color + "\">" + message.msg + "</td></tr>")
        document.getElementById("msgtext").scrollIntoView(false)
      else
        console.log(message)

  $("#msgform").submit (event) ->
    event.preventDefault()
    console.log($("#msgtext").val())
    ws.send(JSON.stringify({msg: $("#msgtext").val()}))
    # reset the form
    $("#msgtext").val("")
