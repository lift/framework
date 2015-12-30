"use strict";

describe("Lift jquery", function() {

  beforeEach(function() {
    jasmine.Ajax.install();

    liftJQuery.logError = function(msg) {
      console.log(msg);
    };
  });

  afterEach(function() {
    jasmine.Ajax.uninstall();
  });

  describe("ajaxGet", function() {
    var request;

    beforeEach(function() {
      liftJQuery.ajaxGet("/nowhere", {});
      request = jasmine.Ajax.requests.mostRecent();
    });

    it("sets request method", function() {
      expect(request.method).toBe('GET');
    });

    it("sets X-Requested-With header", function() {
      expect(request.requestHeaders['X-Requested-With']).toBe('XMLHttpRequest');
    });
  });

  describe("ajaxPost", function() {
    var request;

    beforeEach(function() {
      liftJQuery.ajaxPost("/nowhere", {});
      request = jasmine.Ajax.requests.mostRecent();
    });

    it("sets request method", function() {
      expect(request.method).toBe('POST');
    });

    it("sets X-Requested-With header", function() {
      expect(request.requestHeaders['X-Requested-With']).toBe('XMLHttpRequest');
    });
  });

});
