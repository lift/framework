"use strict";

describe("Lift vanilla", function() {

  beforeEach(function() {
    jasmine.Ajax.install();

    liftVanilla.logError = function(msg) {
      console.log(msg);
    };
  });

  afterEach(function() {
    jasmine.Ajax.uninstall();
  });

  describe("ajaxGet", function() {
    var request;

    beforeEach(function() {
      liftVanilla.ajaxGet("/nowhere", {});
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
      liftVanilla.ajaxPost("/nowhere", {});
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
