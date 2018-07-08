"use strict";
////////////////////////////////////////////////////////////////////////
// JSHint configuration                                               //
////////////////////////////////////////////////////////////////////////
/* global engine                                                      */
/* global print                                                       */
/* global midi                                                        */
/* global SCS3M:true                                                  */
/* jshint -W097                                                       */
/* jshint -W084                                                       */
/* jshint laxbreak: true                                              */
/* jshint laxcomma: true                                              */
////////////////////////////////////////////////////////////////////////

// manually test messages
// amidi -p hw:1 -S F00001601501F7 # flat mode
// amidi -p hw:1 -S 900302 # 90: note on, 03: id of a touch button, 02: red LED

SCS3M = {
    // The device remembers the selected EQ/FX mode per deck
    // and switches to that mode on deck-switch. Set this to
    // false if you prefer the mode to stay the same on
    // deck-switch.
    eqModePerDeck: true,
};

SCS3M.init = function(id) {
    // For reasons unclear to me, errors in init()
    // are not reported
    // Workaround: do the init later
    timer = engine.beginTimer(100, function() {
        engine.stopTimer(timer);
        var device = SCS3M.Device();
        var backend =  SCS3M.Backend(engine);
        SCS3M.agent = SCS3M.Agent(device, backend);

        SCS3M.agent.start();
    });
};

SCS3M.shutdown = function() {
    SCS3M.agent.stop();
};

SCS3M.receive = function(channel, control, value, status) {
    SCS3M.agent.receive(status, control, value);
};

/* Midi map of the SCS.3m device
 *
 * Thanks to Sean M. Pappalardo for the original SCS3 mappings.
 */
SCS3M.Device = function() {
    var NoteOn = 0x90;
    var NoteOff = 0x80;
    var CC = 0xB0;
    var CM = 0xBF; /* this is used for slider mode changes (absolute/relative, sending a control change on channel 16!?) */

    var black = 0x00;
    var blue = 0x01;
    var red = 0x02;
    var purple = blue | red;

    function Logo() {
        var id = 0x69;
        return {
            on: [NoteOn, id, 0x01],
            off: [NoteOn, id, 0x00]
        };
    }

    function Meter(id, lights) {
        function plain(value) {
            if (value <= 0.0) return 1;
            if (value >= 1.0) return lights;
            return 1 + Math.round(value * (lights - 1));
        }

        function clamped(value) {
            if (value <= 0.0) return 1;
            if (value >= 1.0) return lights;
            return Math.round(value * (lights - 2) + 1.5);
        }

        function zeroclamped(value) {
            if (value <= 0.0) return 0;
            if (value >= 1.0) return lights;
            return Math.round(value * (lights - 1) + 0.5);
        }
        return {
            needle: function(value) {
                return [CC, id, plain(value)];
            },
            centerbar: function(value) {
                return [CC, id, 0x14 + clamped(value)];
            },
            bar: function(value) {
                return [CC, id, 0x28 + zeroclamped(value)];
            },
            expand: function(value) {
                return [CC, id, 0x3C + zeroclamped(value)];
            },
        };
    }

    function Slider(id, lights, fields) {
        var touchfields = {};
        for (var fieldn in fields) {
            touchfields[fieldn] = {
                touch: [NoteOn, fields[fieldn]],
                release: [NoteOff, fields[fieldn]],
            };
        }
        return {
            meter: Meter(id, lights),
            slide: [CC, id],
            mode: {
                absolute: [[CM, id, 0x70], [CM, id, 0x7F]],
                relative: [[CM, id, 0x71], [CM, id, 0x7F]],
            },
            field: touchfields,
        };
    }

    function Light(id) {
        return {
            black: [NoteOn, id, black],
            blue: [NoteOn, id, blue],
            red: [NoteOn, id, red],
            purple: [NoteOn, id, purple],
        };
    }

    function Touch(id) {
        return {
            light: Light(id),
            touch: [NoteOn, id],
            release: [NoteOff, id]
        };
    }

    function Side(side) {
        function either(left, right) {
            return ('left' === side) ? left : right;
        }

        function Deck() {
            // The left deck button has a higher address than the right button,
            // for all the other controls the left one has a lower address.
            // I wonder why.
            var id = either(0x10, 0x0F);
            return {
                light: function(bits) {
                    return [NoteOn, id, (bits[0] ? 1 : 0) | (bits[1] ? 2 : 0)];
                },
                touch: [NoteOn, id],
                release: [NoteOff, id]
            };
        }

        function Pitch() {
            return Slider(either(0x00, 0x01), 7, {
                left:   either(0x51, 0x54),
                middle: either(0x52, 0x55),
                right:  either(0x53, 0x56),
            });
        }

        function Eq() {
            return {
                low: Slider(either(0x02, 0x03), 7),
                mid: Slider(either(0x04, 0x05), 7),
                high: Slider(either(0x06, 0x07), 7),
            };
        }

        function Modes() {
            return {
                fx: Touch(either(0x0A, 0x0B)),
                eq: Touch(either(0x0C, 0x0D))
            };
        }

        function Gain() {
            return Slider(either(0x08, 0x09), 7);
        }

        function Touches() {
            return [
                Touch(either(0x00, 0x01)),
                Touch(either(0x02, 0x03)),
                Touch(either(0x04, 0x05)),
                Touch(either(0x06, 0x07)),
            ];
        }

        function Phones() {
            return Touch(either(0x08, 0x09));
        }

        return {
            deck: Deck(),
            pitch: Pitch(),
            eq: Eq(),
            modes: Modes(),
            gain: Gain(),
            touches: Touches(),
            phones: Phones(),
            meter: Meter(either(0x0C, 0x0D), 7)
        };
    }

    return {
        factory: [0xF0, 0x00, 0x01, 0x60, 0x40, 0xF7],
        flat: [0xF0, 0x00, 0x01, 0x60, 0x15, 0x01, 0xF7],
        lightsoff: [CC, 0x7B, 0x00],
        logo: Logo(),
        left: Side('left'),
        right: Side('right'),
        master: Touch(0x0E),
        crossfader: Slider(0x0A, 11)
    };
};



SCS3M.Backend = function(engine) {
    var Control = function(address, name) {
        var control = {};

        control.id = address + "." + name;

        // Absolute
        control.set = function(value) {
            engine.setParameter(address, name,
                value / 127
            );
        };

        control.setValue = function(value) {
            engine.setValue(address, name, value);
        };

        // Relative
        control.varbudge = function(factor) {
            var mult = factor / 128;

            return function(offset) {
                engine.setValue(address, name,
                    engine.getValue(address, name) + (offset - 64) * mult
                );
            };
        };

        control.budge = control.varbudge(1);

        control.fix = function(value) {
            return function() {
                engine.setParameter(address, name, value);
            };
        };

        control.reset = function() {
            engine.reset(address, name);
        };

        control.toggle = function() {
            engine.setValue(address, name, !engine.getValue(address, name));
        };

        control.value = function() {
            return engine.getParameter(address, name);
        };

        control.watch = function(handler) {
            engine.connectControl(address, name, handler);
        };

        control.trigger = function() {
            engine.trigger(address, name);
        };

        return control;
    };

    var EffectSelector = function(address) {
        return (
            { 'next': Control(address, 'next_effect').fix(1)
            , 'prev': Control(address, 'prev_effect').fix(1)
            }
        );
    };

    var Effect = function(base, name) {
        // Example: [EffectRack1_EffectUnit1_Effect1]
        var address = '[' + base + '_' + name + ']';
        var effect = {};

        effect.selector = EffectSelector(address);
        effect.enabled = Control(address, 'enabled');
        effect.meta = Control(address, 'meta');
        effect.superknob = Control(address, 'super1');

        effect.parameter1 = Control(address, 'parameter1');
        effect.parameter2 = Control(address, 'parameter2');
        effect.parameter3 = Control(address, 'parameter3');

        return effect;
    };

    var Chain = function(root, unitaddress) {
        var base = root + '_' + unitaddress;

        // Example: [EffectRack1_EffectUnit1]
        var address = '[' + base + ']';

        var chain = {};

        chain.mix = Control(address, 'mix');
        chain.superknob = Control(address, 'super1');
        chain.effects =
            [ Effect(base, 'Effect1')
            , Effect(base, 'Effect2')
            , Effect(base, 'Effect3')
            , Effect(base, 'Effect4')
            ];

        chain.enable = function(channel) {
            return Control(address, 'group_' + channel.address + '_enable');
        };

        return chain;
    };

    var Channel = function(base) {
        var address = '[' + base + ']';
        var channel = {};
        channel.address = address;

        channel.pregain = Control(address, 'pregain');
        channel.volume = Control(address, 'volume');
        channel.pfl = Control(address, 'pfl');
        channel.vumeter = Control(address, 'VuMeter');
        channel.vumeter_l = Control('[Master]', 'VuMeterL');
        channel.vumeter_r = Control('[Master]', 'VuMeterR');

        // Example address: [QuickEffectRack1_[Channel1]]
        channel.quickeffect = Effect('QuickEffectRack1', address);

        // Example address: [EqualizerRack1_[Channel1]_Effect1]
        channel.eq = Effect("EqualizerRack1", address + "_Effect1");

        channel.beat_active = Control(address, "beat_active");
        channel.playposition = Control(address, "playposition");

        return channel;
    };

    var backend = {};
    backend.channels =
        { '1': Channel('Channel1')
        , '2': Channel('Channel2')
        , '3': Channel('Channel3')
        , '4': Channel('Channel4')
        , 'headphone': Channel('Headphone')
        , 'master': Channel('Master')
        };

    backend.chains =
        [ Chain('EffectRack1', 'EffectUnit1')
        , Chain('EffectRack1', 'EffectUnit2')
        , Chain('EffectRack1', 'EffectUnit3')
        , Chain('EffectRack1', 'EffectUnit4')
        ];

    backend.crossfader = Control('[Master]', 'crossfader');
    backend.headmix    = Control('[Master]', 'headMix');
    backend.headvolume = Control('[Master]', 'headVolume');
    backend.balance    = Control('[Master]', 'balance');

    backend.overload = Control('[Master]', 'audio_latency_overload');

    // HACK HACK HACK This control is used to sync deck state with SCS3d devices
    backend.deck_sync = Control('[PreviewDeck1]', 'quantize');

    return backend;
};



// debugging helper
var printmess = function(message, text) {
    var i;
    var s = '';

    for (i in message) {
        s = s + ('0' + message[i].toString(16)).slice(-2);
    }
    print("Midi " + s + (text ? ' ' + text : ''));
};



SCS3M.Agent = function(device, backend) {
    // Cache last sent bytes to avoid sending duplicates.
    // The second byte of each message (controller id) is used as key to hold
    // the last sent message for each controller.
    var last = {};

    // Queue of messages to send delayed after modeset()
    var loading = false;
    var throttling = false;
    var pipe = [];
    var modepipe = [];

    // Handlers for received messages
    var receivers = {};

    // Connected engine controls
    var watched = {};

    function clear() {
        receivers = {};
        pipe = [];
        modepipe = [];

        // I'd like to disconnect everything on clear, but that doesn't work when using closure callbacks, I guess I'd have to pass the callback function as string name
        // I'd have to invent function names for all handlers
        // Instead I'm not gonna bother and just let the callbacks do nothing
        for (var ctrl in watched) {
            if (watched.hasOwnProperty(ctrl)) {
                watched[ctrl] = [];
            }
        }
    }

    // This function receives Midi messages from the SCS.3m
    // See the XML mapping for all the messages caught
    function receive(type, control, value) {
        var address = (type << 8) + control;
        var handler = receivers[address];
        if (handler) {
            handler(value);
        }
    }

    // Register a handler to listen for messages
    // control: an array with at least two message bytes (type and control id)
    // handler: callback function that will be called each time a value is received
    function expect(control, handler) {
        var address = (control[0] << 8) + control[1];
        receivers[address] = handler;
    }

    function watchRegister(control, handler) {
        if (!watched[control.id]) {
            watched[control.id] = [];
            control.watch(function() {
                var handlers = watched[control.id];
                for (var i in handlers) {
                    handlers[i]();
                }
            });
        }

        watched[control.id].push(handler);
    }

    // Register a handler for changes in engine values
    // This is an abstraction over engine.getParameter()
    function watch(control, handler) {
        watchRegister(control, function() {
            handler(control.value());
        });

        if (loading) {
            // ugly UGLY workaround
            // The device does not light meters again if they haven't changed from last value before resetting flat mode
            // so we send each control some bullshit values which causes awful flicker during startup
            // The trigger will then set things straight
            handler(100);
            handler(-100);
        }

        control.trigger();
    }

    // Register a handler for multiple engine values. It will be called
    // everytime one of the values changes.
    // controls: list of controls to watch
    // handler: will receive list of control values as parameter in same order
    function watchmulti(controls, handler) {
        var values = [];
        var watchControl = function(controlpos, control) {
            values[controlpos] = control.value();
            watchRegister(control, function() {
                values[controlpos] = control.value();
                handler(values);
            });
        };

        for (var i in controls) {
            watchControl(i, controls[i]);
        }
        handler(values);
    }


    // Send MIDI message to device
    // Param message: list of three MIDI bytes
    // Param force: send value regardless of last recorded state
    // Returns whether the massage was sent
    // False is returned if the message was sent before.
    function send(message, force, extra) {
        if (!message){
            print("SCS3 warning: send function received invalid message");
            return; // :-(
        }

        var address = (message[0] << 8) + message[1];

        if (!force && last[address] === message[2]) {
            return false; // Not repeating same message
        }

        midi.sendShortMsg(message[0], message[1], message[2]);

        last[address] = message[2];

        return true;
    }

    // Wrapper function for send() that delays messages after modesetting
    function tell(message) {
        if (throttling) {
            pipe.push(message);
            return;
        }

        send(message);
    }

    // Send MIDI mode messages to device
    // Param messages: list of two messages three byte values each
    // Returns whether the massage was sent
    function sendmode(messages) {
        // Modeset messages are comprised of the actual modeset message and
        // a termination message that must be sent after.
        var mode= messages[0];
        var termination = messages[1];

	// Special SYSEX special messages are special
        if (mode.length > 3) {
            midi.sendSysexMsg(mode, mode.length);
            return true;
        }

        var address = (mode[0] << 8) + mode[1];

        if (last[address] === mode[2]) {
            return false; // Not repeating same message
        }

        midi.sendShortMsg(mode[0], mode[1], mode[2]);
        midi.sendShortMsg(termination[0], termination[1], termination[2]);

        // Record message as sent
        last[address] = mode[2];

        return true;
    }

    // Send modeset messages to the device
    //
    // messages: list of one or two messages to send
    //
    // Either provide a pair of messages to set a slider to a different
    // mode, or send just one long message in the list. Transmission of
    // subsequent messages will be delayed to give the device some time to apply
    // the changes.
    function modeset(messages) {
        if (throttling) {
            modepipe.push(messages);
            return;
        }

        var sent = sendmode(messages);

        if (sent) {
            // after modesetting, we have to wait for the device to settle
            throttling = engine.beginTimer(0, flush);
        }
    }

    var flush = function() {
        var mode = modepipe.shift();
      
        if (mode) {
            sendmode(mode);
            return;
        }

        // Now we can flush the rest of the messages.
        // On init, some controls are left unlit if the messages are sent
        // without delay. The causes are unclear. Sending only a few messages
        // per tick seems to work ok.
        var message;
        var limit = 5; // Determined experiementally
        while (pipe.length) {
            message = pipe.shift();
            send(message);
            if (loading && limit-- < 1) return;
        }

        if (throttling) engine.stopTimer(throttling);
        throttling = false;
        loading = false;
    };

    // Map engine values in the range [0..1] to lights
    // translator maps from [0..1] to a midi message (three bytes)
    function patch(translator) {
        if (typeof translator !== 'function') {
            throw "ParamError: Expected function, got " + translator;
        }
        return function(value) {
            tell(translator(value));
        };
    }

    // Cut off at 0.01 because it drops off very slowly
    function vupatch(translator) {
        return function(value) {
            value = value * 1.01 - 0.01;
            tell(translator(value));
        };
    }

    // accelerate away from 0.5 so that small changes become visible faster
    function offcenter(translator) {
        return function(value) {
            // If you want to adjust it, fiddle with the exponent (second argument to pow())
            return translator(Math.pow(Math.abs(value - 0.5) * 2, 0.6) / (value < 0.5 ? -2 : 2) + 0.5);
        };
    }

    function binarylight(off, on) {
        return function(value) {
            tell(value ? on : off);
        };
    }

    function Switch(off, on) {
        var engaged = false;

        return {
            'change': function(state) {
                state = !!(state);
                var changed = engaged !== state;
                engaged = state;
                return changed;
            },
            'engage': function() {
                engaged = true;
            },
            'cancel': function() {
                engaged = false;
            },
            'toggle': function() {
                engaged = !engaged;
            },
            'engaged': function() {
                return engaged ? on : off;
            },
            'choose': function(off, on) {
                return engaged ? on : off;
            }
        };
    }


    // HoldDelayedSwitches can be engaged, and they can be held.
    // A switch that is held for less than 200 ms will toggle.
    // After 200ms it will enter held-mode.
    function HoldDelayedSwitch(off, on) {
        var sw = Switch(off, on);

        var held = false;
        var heldBegin = false;

        sw.hold = function(onHeld) {
            return function() {
                heldBegin = true;
                var switchExpire = engine.beginTimer(200, function() {
                    engine.stopTimer(switchExpire);
                    if (heldBegin) {
                        heldBegin = false;
                        held = true;
                        if (onHeld) onHeld();
                    }
                });
            };
        };

        sw.release = function() {
            if (heldBegin) sw.toggle();
            held = false;
            heldBegin = false;
        };

        sw.held = function() {
            return held;
        };

        return sw;
    }

    var MultiSwitch = function(preset) {
        var current = preset;
        var held = false;
        var timer = false;
        var next = false;

        return {
            'hold': function(mode, onHeld) {
                return function() {
                    next = mode;
                    if (timer) engine.stopTimer(timer);
                    timer = engine.beginTimer(200, function() {
                        engine.stopTimer(timer);
                        timer = false;
                        held = next;
                        if (onHeld) onHeld();
                    });
                }
            },

            'release': function() {
                if (timer) {
                    engine.stopTimer(timer);
                    timer = false;
                    current = next;
                }
                held = false;
            },

            'engaged': function(mode) { return mode === current; },
            'held': function(mode) { return mode === held; },
            'either': function(mode, forOn, forOff) {
                if (mode === current) return forOn;
                return forOff;
            },
            'choice': function(mode, forOff, forOn, forHeld) {
                if (mode === held) return forHeld;
                if (mode === current) return forOn;
                return forOff;
            }
        };
    }

    var master = Switch(); // Whether master key is held
    var deck = {
        left: HoldDelayedSwitch(1, 3), // off: channel1, on: channel3
        right: HoldDelayedSwitch(2, 4) // off: channel2, on: channel4
    };

    var overlayA = MultiSwitch('eq');
    var overlayB = MultiSwitch('eq');
    var overlayC;
    var overlayD;
    if (SCS3M.eqModePerDeck) {
        overlayC = MultiSwitch('eq');
        overlayD = MultiSwitch('eq');
    } else {
        overlayC = overlayA;
        overlayD = overlayB;
    }

    var overlay = {
        left: [overlayA, overlayC],
        right: [overlayB, overlayD],
    };

    function remap() {
        clear();
        patchage();
    }

    // Remap for chainig with handlers
    function repatch(handler) {
        return function(value) {
            var ret = handler(value);
            remap();
            return ret;
        };
    }

    function patchage() {

        function Side(side) {
            var part = device[side];
            var deckside = deck[side];

            // Switch deck/channel when button is touched
            expect(part.deck.touch, deckside.hold(remap));
            expect(part.deck.release, repatch(deckside.release));

            function either(left, right) {
                return (side === 'left') ? left : right;
            }

            var channelno = deck[side].choose(either(1, 2), either(3, 4));
            var channel = backend.channels[channelno];
            var chain = backend.chains[channelno - 1];
            var sideoverlay = overlay[side][deckside.choose(0, 1)];

            // Light the corresponding deck (channel 1: A, channel 2: B, channel 3: C, channel 4: D)
            // Make the lights blink on each beat
            function beatlight(translator, activepos, held) {
                return function(bits) {
                    bits = bits.slice(); // clone
                    if (held) {
                        // When the switch his held, light both LED
                        // turn them off when beat is active
                        bits[0] = !bits[0];
                        bits[1] = !bits[1];
                    } else {
                        // Invert the bit for the light that should be on
                        bits[activepos] = !bits[activepos];
                    }
                    return translator(bits);
                };
            }
            watchmulti([
                backend.channels[either(1, 2)].beat_active,
                backend.channels[either(3, 4)].beat_active,
            ], patch(beatlight(part.deck.light, deckside.choose(0, 1), deckside.held())));

            expect(part.modes.eq.touch, sideoverlay.hold('eq', remap));
            expect(part.modes.eq.release, repatch(sideoverlay.release));
            tell(part.modes.eq.light[sideoverlay.choice('eq', 'blue', 'red', 'purple')]);
            if (sideoverlay.engaged('eq')) {
                modeset(part.eq.low.mode.relative);
                modeset(part.eq.mid.mode.relative);
                modeset(part.eq.high.mode.relative);
                var op = sideoverlay.choice('eq', false, 'budge', 'reset');
                expect(part.eq.low.slide, channel.eq.parameter1[op]);
                expect(part.eq.mid.slide, channel.eq.parameter2[op]);
                expect(part.eq.high.slide, channel.eq.parameter3[op]);

                watch(channel.eq.parameter1, patch(offcenter(part.eq.low.meter.centerbar)));
                watch(channel.eq.parameter2, patch(offcenter(part.eq.mid.meter.centerbar)));
                watch(channel.eq.parameter3, patch(offcenter(part.eq.high.meter.centerbar)));

                if (!master.engaged()) {
                    modeset(part.pitch.mode.relative);
                    expect(part.pitch.slide, sideoverlay.choice('eq', false,
                        channel.quickeffect.superknob.varbudge(0.5),
                        channel.quickeffect.superknob.reset
                    ));
                    watch(channel.quickeffect.superknob, offcenter(patch(part.pitch.meter.centerbar)));
                }
            } else {
                if (!master.engaged()) {
                    modeset(part.pitch.mode.absolute);
                    expect(part.pitch.slide, sideoverlay.held('eq') ? chain.superknob.reset : chain.superknob.set);
                    watch(chain.superknob, patch(part.pitch.meter.needle));
                }
            }

            expect(part.modes.fx.touch, sideoverlay.hold('fx', remap));
            expect(part.modes.fx.release, repatch(sideoverlay.release));
            tell(part.modes.fx.light[sideoverlay.choice('fx', 'blue', 'red', 'purple')]);
            if (sideoverlay.engaged('fx')) {
                modeset(part.eq.low.mode.absolute);
                modeset(part.eq.mid.mode.absolute);
                modeset(part.eq.high.mode.absolute);
                var op = sideoverlay.choice('eq', 'set', false, 'reset');
                expect(part.eq.low.slide, chain.effects[0].meta[op]);
                expect(part.eq.mid.slide, chain.effects[1].meta[op]);
                expect(part.eq.high.slide, chain.effects[2].meta[op]);

                watch(chain.effects[0].meta, patch(part.eq.low.meter.needle));
                watch(chain.effects[1].meta, patch(part.eq.mid.meter.needle));
                watch(chain.effects[2].meta, patch(part.eq.high.meter.needle));
            }

            var touchMap = function(tnr) {
                var effect = chain.effects[tnr];
                var softbutton = part.touches[tnr];
                var held = sideoverlay.held(tnr);
                var enabled = effect.enabled.value();


                if (sideoverlay.held('fx')) {
                    expect(softbutton.touch, effect.enabled.toggle);
                    tell(softbutton.light[enabled ? 'red' : 'black']);
                } else {
                    expect(softbutton.touch, sideoverlay.hold(tnr, remap));
                    tell(softbutton.light[sideoverlay.choice(tnr, 
                        enabled ? 'blue' : 'black', // not active
                        enabled ? 'purple' : 'red', // active
                        'purple' // held
                        )
                    ]);
                }

                expect(softbutton.release, repatch(sideoverlay.release));
                if (sideoverlay.engaged(tnr)) {
                    var op = sideoverlay.choice('eq', 'set', false, 'reset');
                    modeset(part.eq.low.mode.absolute);
                    modeset(part.eq.mid.mode.absolute);
                    modeset(part.eq.high.mode.absolute);
                    expect(part.eq.low.slide, effect.parameter1[op]);
                    expect(part.eq.mid.slide, effect.parameter2[op]);
                    expect(part.eq.high.slide, effect.parameter3[op]);
                    watch(effect.parameter1, patch(offcenter(part.eq.low.meter.centerbar)));
                    watch(effect.parameter2, patch(offcenter(part.eq.mid.meter.centerbar)));
                    watch(effect.parameter3, patch(offcenter(part.eq.high.meter.centerbar)));
                }

                // Select effect by touching top slider when button is held
                if (held) {
                    tell(part.pitch.meter.expand(0.3));
                    expect(
                        part.pitch.field.left.touch,
                        effect.selector.next
                    );
                    expect(
                        part.pitch.field.right.touch,
                        effect.selector.prev
                    );
                }
            };

            for (var tnr = 0; tnr < 4; tnr++) {
                touchMap(tnr);
            }

            if (!master.engaged()) {
                if (deckside.held()) {
                    modeset(part.gain.mode.relative);
                    expect(part.gain.slide, eqsideheld.choose(
                        channel.pregain.budge,
                        channel.pregain.reset
                    ));
                    watch(channel.pregain, patch(offcenter(part.gain.meter.needle)));
                } else {
                    modeset(part.gain.mode.absolute);
                    expect(part.gain.slide, channel.volume.set);
                    watch(channel.volume, patch(part.gain.meter.bar));
                }
            }

            watch(channel.pfl, binarylight(part.phones.light.blue, part.phones.light.red));
            expect(part.phones.touch, channel.pfl.toggle);

            if (!master.engaged()) {
                watch(channel.vumeter, vupatch(part.meter.bar));
            }
        }

        // Light the logo and let it go out to signal an overload
        watch(backend.overload, binarylight(
            device.logo.on,
            device.logo.off
        ));
        Side('left');
        Side('right');

        tell(device.master.light[master.choose('blue', 'purple')]);
        expect(device.master.touch, repatch(master.engage));
        expect(device.master.release, repatch(master.cancel));
        if (master.engaged()) {
            modeset(device.left.pitch.mode.absolute);
            watch(backend.headmix, patch(device.left.pitch.meter.centerbar));
            expect(device.left.pitch.slide,
                eqheld.left.engaged() ? backend.headmix.reset : backend.headmix.set
            );

            modeset(device.right.pitch.mode.absolute);
            watch(backend.balance, patch(device.right.pitch.meter.centerbar));
            expect(device.right.pitch.slide,
                eqheld.right.engaged() ? backend.balance.reset : backend.balance.set
            );

            modeset(device.left.gain.mode.relative);
            watch(backend.headvolume, patch(device.left.gain.meter.centerbar));
            expect(device.left.gain.slide, backend.headvolume.budge);

            var masterch = backend.channels.master;
            modeset(device.right.gain.mode.relative);
            watch(masterch.volume, patch(device.right.gain.meter.centerbar));
            expect(device.right.gain.slide, masterch.volume.budge);

            watch(masterch.vumeter_l, vupatch(device.left.meter.bar));
            watch(masterch.vumeter_r, vupatch(device.right.meter.bar));
        }

        var crossfade = function() {
            expect(device.crossfader.slide, backend.crossfader.set);
            watch(backend.crossfader, patch(device.crossfader.meter.centerbar));
        };

        var needledrop = function(channel) {
            expect(device.crossfader.slide, channel.playposition.set);
            watch(channel.playposition, patch(device.crossfader.meter.needle));
        };

        if (deck.left.held()) {
            needledrop(backend.channels[deck.left.engaged()]);
        } else if (deck.right.held()) {
            needledrop(backend.channels[deck.right.engaged()]);
        } else {
            crossfade();
        }

        // Communicate currently selected channel of each deck so SCS3d can read it
        // THIS USES A CONTROL FOR ULTERIOR PURPOSES AND IS VERY NAUGHTY INDEED
        backend.deck_sync.setValue(
            0x4 // Setting bit three communicates that we're sending deck state
            | deck.left.choose(0, 1) // left side is in bit one
            | deck.right.choose(0, 2) // right side bit two
        );
        watch(backend.deck_sync, function(deckState) {
            var changed = deck.left.change(deckState & 1) || deck.right.change(deckState & 2);
            if (changed) repatch(function() {})();
        });
    }



    return {
        start: function() {
            loading = true;
            modeset([device.flat]);
            patchage();
        },
        receive: receive,
        stop: function() {
            clear();
            tell(device.lightsoff);
            send(device.logo.on, true);
        }
    };
};
