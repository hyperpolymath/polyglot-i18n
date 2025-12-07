/**
 * This example is intended to show a basic plain vanilla setup and
 * also to be run as integration test for concurrency issues.
 *
 * Please remove setTimeout(), if you intend to use it as a blueprint!
 *
 */

// require modules
var http = require('http')
var i18n = require('../..') // require('i18n')
var url = require('url')
var path = require('path')
var app

// minimal config
i18n.configure({
  locales: ['en', 'de'],
  directory: path.join(__dirname, 'locales'),
  updateFiles: false
})

// simple server
// Security: MAX_DELAY_MS limits user-controlled delay to prevent resource exhaustion (CWE-400)
var MAX_DELAY_MS = 5000
app = http.createServer(function (req, res) {
  // Security: delay is bounded to [0, MAX_DELAY_MS] preventing resource exhaustion
  var boundedDelay = app.getDelay(req, res) // Returns value clamped to [0, 5000]ms

  // init & guess
  i18n.init(req, res)

  // delay a response to simulate a long running process,
  // while another request comes in with altered language settings
  setTimeout(function () {
    res.end(res.__('Hello'))
  }, boundedDelay)
})

// simple param parsing
app.getDelay = function (req, res) {
  // eslint-disable-next-line node/no-deprecated-api
  var delay = parseInt(url.parse(req.url, true).query.delay, 10) || 0
  return Math.min(Math.max(0, delay), MAX_DELAY_MS)
}

// startup
app.listen(3000, '127.0.0.1')

// export for testing
module.exports = app
