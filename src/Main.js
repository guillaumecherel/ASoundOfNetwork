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
      , knock = createKnockFilter(context)
      , gain = context.createGain();

    connect(knock, gain, context.destination);
    gain.gain.value = gainValue || 20;

    this.context = context;
    this.tick = function(duration) {
        var source = createImpulseSource(this.context, duration || 1.0);

        source.connect(knock);
        source.start();
    }
}

function connect(/* node1 -> node2 -> ... -> nodeN */) {
    for (var len = arguments.length, i = 1; i < len; i++)
        arguments[i-1].connect(arguments[i]);
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
      , impulseFrameCount = 5
      , rate = context.sampleRate
      , delta = Math.floor(Math.random() * 6 - 3);

    duration = Math.max(duration, 1.0 / rate * impulseFrameCount);
    buffer = context.createBuffer(channels, duration * rate, rate);

    for (var channel = 0; channel < channels; channel++) {
        var i, data = buffer.getChannelData(channel);

        for (i = 0; i < buffer.length; i++)
            data[i] = 0;

        pos = Math.floor(0.1 * duration * (rate / spacing));
        for (i = 0; i < impulseFrameCount; i++)
            data[pos * spacing + i + delta] = 1;
    }

    source = context.createBufferSource();
    source.buffer = buffer;

    return source;
}
