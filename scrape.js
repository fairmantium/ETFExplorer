var url ='https://investor.vanguard.com/etf/list#/mutual-funds/asset-class/month-end-returns';
    var page = new WebPage();
    var fs = require('fs');
    page.open(url, function (status) {
    just_wait();
    });
    function just_wait() {
    setTimeout(function() {
    fs.write('1.html', page.content, 'w');
    phantom.exit();
    }, 2500);
    }
    
