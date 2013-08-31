/* ***** BEGIN LICENSE BLOCK *****
 * Distributed under the BSD license:
 *
 * Copyright (c) 2012, Ajax.org B.V.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Ajax.org B.V. nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL AJAX.ORG B.V. BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *
 * Contributor(s):
 *
 *
 *
 * ***** END LICENSE BLOCK ***** */

ace.define('ace/mode/oryza_dat', ['require', 'exports', 'module' , 'ace/lib/oop', 'ace/mode/text', 'ace/tokenizer', 'ace/mode/oryza_dat_highlight_rules', 'ace/mode/folding/cstyle'], function(require, exports, module) {


var oop = require("../lib/oop");
var TextMode = require("./text").Mode;
var Tokenizer = require("../tokenizer").Tokenizer;
var IniHighlightRules = require("./oryza_dat_highlight_rules").IniHighlightRules;
var FoldMode = require("./folding/cstyle").FoldMode;

var Mode = function() {
    var highlighter = new IniHighlightRules();
    this.foldingRules = new FoldMode();
    this.$tokenizer = new Tokenizer(highlighter.getRules());
};
oop.inherits(Mode, TextMode);

(function() {
    this.lineCommentStart = ";";
    this.blockComment = {start: "/*", end: "*/"};
}).call(Mode.prototype);

exports.Mode = Mode;
});

ace.define('ace/mode/oryza_dat_highlight_rules', ['require', 'exports', 'module' , 'ace/lib/oop', 'ace/mode/text_highlight_rules'], function(require, exports, module) {


var oop = require("../lib/oop");
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var OryzaDatHighlightRules = function() {

    this.$rules = {
        "start" : [
            {
                // push <<comment>>
                token : "comment",
                regex : /[*!]/,
                next  : "comment"
            }, {
                // eat whitespace
                token : "keyword.operator",
                regex : /[=]\s*/,
                next  : "value"
            }, {
                defaultToken: "variable"
            }
        ],
        "value" : [
            {
                // push <<comma>>
                token : "punctuation.operator",
                regex : /[,]\s*/,
                next  : "cvalue"
            }, {
                // push <<squote>>
                token : "string",
                regex : /[']/,
                next  : "qstring"
            }, {
                // push <<dquote>>
                token : "string",
                regex : /["]/,
                next  : "qqstring"
            }, {
                // push <<comment>>
                token : "comment",
                regex : /[*!]/,
                next  : "comment"
            }, {
                token : "value",
                regex : /$/,
                next  : "start"
            }, {
                // eat whitespace
                token : "value",
                regex : /\s/
            }, {
                defaultToken : "value"
            }
        ],
        "cvalue" : [ // just like "value", but <<comma>> stack is not empty
            {
                // push <<comma>>, technically NOOP, also eat whitespace
                token : "punctuation.operator",
                regex : /[,]\s*/,
                next  : "cvalue"
            }, {
                // push <<squote>>
                token : "string",
                regex : /[']/,
                next  : "qstring"
            }, {
                // push <<dquote>>
                token : "string",
                regex : /["]/,
                next  : "qqstring"
            }, {
                // push <<comment>>
                token : "comment",
                regex : /[*!]/,
                next  : "ccomment"
            }, {
                token : "value",
                regex : /$/,
                next  : "start"
            }, {
                // any other match pops <<comma>>
                defaultToken : "value"
            }
        ],
        "qstring" : [
            {
                // pop <<squote>>
                token : "string",
                regex : /[']/,
                next  : "value"
            }, {
                defaultToken : "string"
            }
        ],
        "qqstring" : [
            {
                // pop <<dquote>>
                token : "string",
                regex : /["]/,
                next  : "value"
            }, {
                defaultToken : "string"
            }
        ],
        "comment" : [
            {
                // pop <<comment>>
                token : "comment",
                regex : /$/,
                next  : "start"
            }, {
                defaultToken : "comment"
            }
        ],
        "ccomment" : [ // just like "comment", but <<comma>> stack is not empty
            {
                // pop <<comment>>
                token : "comment",
                regex : /$/,
                next  : "value"
            }, {
                defaultToken : "comment"
            }
        ]
    };
    
    this.normalizeRules();
};

OryzaDatHighlightRules.metaData = { fileTypes: [ 'dat' ],
      keyEquivalent: '^~D',
      name: 'Dat',
      scopeName: 'source.dat' }


oop.inherits(OryzaDatHighlightRules, TextHighlightRules);

exports.IniHighlightRules = OryzaDatHighlightRules;
});

ace.define('ace/mode/folding/cstyle', ['require', 'exports', 'module' , 'ace/lib/oop', 'ace/range', 'ace/mode/folding/fold_mode'], function(require, exports, module) {


var oop = require("../../lib/oop");
var Range = require("../../range").Range;
var BaseFoldMode = require("./fold_mode").FoldMode;

var FoldMode = exports.FoldMode = function(commentRegex) {
    if (commentRegex) {
        this.foldingStartMarker = new RegExp(
            this.foldingStartMarker.source.replace(/\|[^|]*?$/, "|" + commentRegex.start)
        );
        this.foldingStopMarker = new RegExp(
            this.foldingStopMarker.source.replace(/\|[^|]*?$/, "|" + commentRegex.end)
        );
    }
};
oop.inherits(FoldMode, BaseFoldMode);

(function() {

    this.foldingStartMarker = /(\{|\[)[^\}\]]*$|^\s*(\/\*)/;
    this.foldingStopMarker = /^[^\[\{]*(\}|\])|^[\s\*]*(\*\/)/;

    this.getFoldWidgetRange = function(session, foldStyle, row) {
        var line = session.getLine(row);
        var match = line.match(this.foldingStartMarker);
        if (match) {
            var i = match.index;

            if (match[1])
                return this.openingBracketBlock(session, match[1], row, i);

            return session.getCommentFoldRange(row, i + match[0].length, 1);
        }

        if (foldStyle !== "markbeginend")
            return;

        var match = line.match(this.foldingStopMarker);
        if (match) {
            var i = match.index + match[0].length;

            if (match[1])
                return this.closingBracketBlock(session, match[1], row, i);

            return session.getCommentFoldRange(row, i, -1);
        }
    };

}).call(FoldMode.prototype);

});
