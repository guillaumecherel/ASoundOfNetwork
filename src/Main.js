exports.tick = function () {
    console.log("Tick!!!");

    if (!window.geiger)
        window.geiger = new Geiger();

    window.geiger.tick();
}

// Inspired by: http://afandian.com/geigor
// See also: https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext/createBufferSource

function Geiger(gainValue) {
    var context = createAudioContext()
      , knock = createKnockFilter(context, {freq: 500, Q: 25})
      , reverb = new SimpleReverb(context, {
                  seconds: 1,
                  decay: 10,
                  reverse: 0
        })
      , gain = context.createGain();

    connect(knock, reverb, gain, context.destination);
    gain.gain.value = gainValue || 100;

    this.context = context;
    this.tick = function(duration) {
        var source = createImpulseSource(this.context, duration || 1.0);

        source.connect(knock);
        source.start();
    }
}

function connect(/* node1 -> node2 -> ... -> nodeN */) {
    for (var len = arguments.length, i = 1; i < len; i++) {
        var src = arguments[i-1], dst = arguments[i];
        (src.output || src).connect(dst.input || dst);
    }
}

function createAudioContext() {
    return new (window.AudioContext || window.webkitAudioContext)();
}

function createKnockFilter(context, opts /* = { type, freq, Q } */) {
    if (opts == null) opts = {};
    if (opts.type == null) opts.type = "bandpass";
    if (opts.freq == null) opts.freq = 450;
    if (opts.Q == null) opts.Q = 10;

    // Filter to make the impulses into 'knock' sounds.
    var filter = context.createBiquadFilter();
    filter.type = opts.type;
    filter.frequency.value = opts.freq;
    filter.Q.value = opts.Q;
    filter.gain.value = 40; // if > 0 doesn't seem to change anything

    return filter;
}

function createImpulseSource(context, duration) {
    var pos, source, buffer
      , channels = 2
      , spacing = 1000.0
      , impulseFrameCount = 1
      , rate = context.sampleRate
      , delta = Math.floor(Math.random() * 6 - 3);

    duration = Math.max(duration, 1.0 / rate * impulseFrameCount);
    buffer = context.createBuffer(channels, duration * rate, rate);

    for (var channel = 0; channel < channels; channel++) {
        var i, data = buffer.getChannelData(channel);

        for (i = 0; i < buffer.length; i++)
            data[i] = 0;

        pos = Math.floor(0.1 * duration * (rate / spacing));
        for (i = 0; i < impulseFrameCount; i++) {
            //data[pos * spacing + i + delta] = 1;
            data[i] = 1;
        }
    }

    source = context.createBufferSource();
    source.buffer = buffer;

    return source;
}






/**
 * https://github.com/web-audio-components/simple-reverb

 * Simple Reverb constructor.
 *
 * @param {AudioContext} context
 * @param {object} opts
 * @param {number} opts.seconds
 * @param {number} opts.decay
 * @param {boolean} opts.reverse
 */

function SimpleReverb (context, opts) {
  this.input = this.output = context.createConvolver();
  this._context = context;

  var p = this.meta.params;
  opts = opts || {};
  this._seconds   = opts.seconds  || p.seconds.defaultValue;
  this._decay     = opts.decay    || p.decay.defaultValue;
  this._reverse   = opts.reverse  || p.reverse.defaultValue;
  this._buildImpulse();
}

SimpleReverb.prototype = Object.create(null, {

  /**
   * AudioNode prototype `connect` method.
   *
   * @param {AudioNode} dest
   */

  connect: {
    value: function (dest) {
      this.output.connect( dest.input ? dest.input : dest );
    }
  },

  /**
   * AudioNode prototype `disconnect` method.
   */

  disconnect: {
    value: function () {
      this.output.disconnect();
    }
  },

  /**
   * Utility function for building an impulse response
   * from the module parameters.
   */

  _buildImpulse: {
    value: function () {
      var rate = this._context.sampleRate
        , length = rate * this.seconds
        , decay = this.decay
        , impulse = this._context.createBuffer(2, length, rate)
        , impulseL = impulse.getChannelData(0)
        , impulseR = impulse.getChannelData(1)
        , n, i;

      for (i = 0; i < length; i++) {
        n = this.reverse ? length - i : i;
        impulseL[i] = (Math.random() * 2 - 1) * Math.pow(1 - n / length, decay);
        impulseR[i] = (Math.random() * 2 - 1) * Math.pow(1 - n / length, decay);
      }

      this.input.buffer = impulse;
    }
  },

  /**
   * Module parameter metadata.
   */

  meta: {
    value: {
      name: "SimpleReverb",
      params: {
        seconds: {
          min: 1,
          max: 50,
          defaultValue: 3,
          type: "float"
        },
        decay: {
          min: 0,
          max: 100,
          defaultValue: 2,
          type: "float"
        },
        reverse: {
          min: 0,
          max: 1,
          defaultValue: 0,
          type: "bool"
        }
      }
    }
  },

  /**
   * Public parameters.
   */

  seconds: {
    enumerable: true,
    get: function () { return this._seconds; },
    set: function (value) {
      this._seconds = value;
      this._buildImpulse();
    }
  },

  decay: {
    enumerable: true,
    get: function () { return this._decay; },
    set: function (value) {
      this._decay = value;
      this._buildImpulse();
    }
  },

  reverse: {
    enumerable: true,
    get: function () { return this._reverse; },
    set: function (value) {
      this._reverse = value;
      this._buildImpulse();
    }
  }

});
