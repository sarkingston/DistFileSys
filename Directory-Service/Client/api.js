
var postFileAdd = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/file/add'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getFileGetByName = function(name, onSuccess, onError)
{
  $.ajax(
    { url: '/file/get/' + encodeURIComponent(name) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
