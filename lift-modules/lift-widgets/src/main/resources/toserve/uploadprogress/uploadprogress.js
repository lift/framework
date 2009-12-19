/*
* jquery.uploadProgress
*
* Copyright (c) 2008 Piotr Sarnacki (drogomir.com)
*
* Licensed under the MIT license:
* http://www.opensource.org/licenses/mit-license.php
* 
* Adapted for the Lift framework by Tim Perrett, 2009.
* 
*/
(function($) {
  $.fn.uploadProgress = function(options) {
  options = $.extend({
    dataType: "json",
    interval: 1500,
    progressBar: "#progressbar",
    progressUrl: "/progress",
    start: function(){},
    uploading: function(){},
    complete: function(){},
    success: function(){},
    error: function(){},
    preloadImages: [],
    uploadProgressPath: '/classpath/uploadprogress/uploadprogress.js',
    jqueryPath: '/classpath/jquery.js',
    timer: ""
  }, options);
  
  $(function() {
    for(var i = 0; i<options.preloadImages.length; i++){
     options.preloadImages[i] = $("<img>").attr("src", options.preloadImages[i]);
    }
    /** tried to add iframe after submit (to not always load it) but it 
     * won't work. safari can't get scripts properly while submitting files 
     */
    if($.browser.safari && top.document == document) {
      /* iframe to send ajax requests in safari thanks to Michele Finotto for idea */
      iframe = document.createElement('iframe');
      iframe.name = "progressFrame";
      $(iframe).css({width: '0', height: '0', position: 'absolute', top: '-3000px'});
      document.body.appendChild(iframe);
      
      var d = iframe.contentWindow.document;
      d.open();
      /* weird - safari won't load scripts without this lines... */
      d.write('<html><head></head><body></body></html>');
      d.close();
      
      var b = d.body;
      var s = d.createElement('script');
      s.src = options.jqueryPath;
      /* must be sure that jquery is loaded */
      s.onload = function() {
        var s1 = d.createElement('script');
        s1.src = options.uploadProgressPath;
        b.appendChild(s1);
      }
      b.appendChild(s);
    }
  });
  
  return this.each(function(){
    $(this).bind('submit', function() {
      /* start callback */
      options.start();
      var uploadProgress = $.browser.safari ? progressFrame.jQuery.uploadProgress : jQuery.uploadProgress;
      options.timer = window.setInterval(function() { uploadProgress(this, options) }, options.interval);
    });
  });
  };
 
jQuery.uploadProgress = function(e, options) {
  jQuery.ajax({
    type: "GET",
    url: options.progressUrl,
    dataType: options.dataType,
    success: function(upload) {
      var bar = $.browser.safari ? $(options.progressBar, parent.document) : $(options.progressBar);
      if (upload.state == 'uploading') {
        upload.percents = Math.floor((upload.received / upload.size)*1000)/10;
        //alert(upload.percentage)
        bar.css({width: upload.percentage+'%'});
        options.uploading(upload);
      }
      if (upload.state == 'completed' || upload.state == 'error'){
        window.clearTimeout(options.timer);
        options.complete(upload);
      }
      if(upload.state == 'completed'){
        bar.css({width:'100%'});
        options.success(upload);
      }
      if(upload.state == 'error'){
        options.error(upload);
      }
    }
  });
};
})(jQuery);