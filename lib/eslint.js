/**
 * @fileoverview Main ESLint object.
 * @author Nicholas C. Zakas
 */
"use strict";

//------------------------------------------------------------------------------
// Requirements
//------------------------------------------------------------------------------

var esprima = require("esprima"),
    estraverse = require("estraverse"),
    escope = require("escope"),
    environments = require("../conf/environments.json"),
    rules = require("./rules"),
    util = require("./util"),
    RuleContext = require("./rule-context"),
    EventEmitter = require("events").EventEmitter,
    cssauron = require("cssauron-esprima");

//------------------------------------------------------------------------------
// Helpers
//------------------------------------------------------------------------------

function escapeRegExp(rx) {
    return rx.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
}

/**
 * Parses a list of "name:boolean_value" or/and "name" options divided by comma or
 * whitespace.
 * @param {string} string The string to parse.
 * @returns {Object} Result map object of names and boolean values
 */
function parseBooleanConfig(string) {
    var items = {};
    // Collapse whitespace around : to make parsing easier
    string = string.replace(/\s*:\s*/g, ":");
    // Collapse whitespace around ,
    string = string.replace(/\s*,\s*/g, ",");
    string.split(/\s|,+/).forEach(function(name) {
        if (!name) {
            return;
        }
        var pos = name.indexOf(":"),
            value;
        if (pos !== -1) {
            value = name.substring(pos + 1, name.length);
            name = name.substring(0, pos);
        }

        items[name] = (value === "true");

    });
    return items;
}

/**
 * Parses a JSON-like config.
 * @param {string} string The string to parse.
 * @returns {Object} Result map object
 */
function parseJsonConfig(string) {
    var items = {};
    string = string.replace(/([a-z0-9\-]+):/g, "\"$1\":").replace(/(\]|[0-9])\s+(?=")/, "$1,");
    try {
        items = JSON.parse("{" + string + "}");
    } catch(e) { }

    return items;
}

/**
 * Parses a config of values separated by comma.
 * @param {string} string The string to parse.
 * @returns {Object} Result map of values and true values
 */
function parseListConfig(string) {
    var items = {};
    // Collapse whitespace around ,
    string = string.replace(/\s*,\s*/g, ",");
    string.split(/,+/).forEach(function(name) {
        name = name.trim();
        if (!name) {
            return;
        }
        items[name] = true;
    });
    return items;
}

/**
 * @param {Scope} scope The scope object to check.
 * @param {string} name The name of the variable to look up.
 * @returns {Variable} The variable object if found or null if not.
 */
function getVariable(scope, name) {
    var variable = null;
    scope.variables.some(function(v) {
        if (v.name === name) {
            variable = v;
            return true;
        } else {
            return false;
        }

    });
    return variable;
}

/**
 * Add or modify list of globals in scope
 * @param {[Object]} globals List name:is_writable of variables
 * @param {Object} scope   The scope object
 * @returns {void}
 */
function addGlobalsToScope(globals, scope){

    Object.keys(globals).forEach(function(name) {
        var variable = getVariable(scope, name);
        if (!variable) {
            variable = new escope.Variable(name, scope);
            scope.variables.push(variable);
        }
        variable.writeable = globals[name];
    });
}

/**
 * Ensures that variables representing built-in properties of the Global Object,
 * and any globals declared by special block comments, are present in the global
 * scope.
 * @param {ASTNode} program The top node of the AST.
 * @param {Scope} globalScope The global scope.
 * @param {Object} config The existing configuration data.
 * @returns {void}
 */
function addDeclaredGlobals(program, globalScope, config) {
    var declaredGlobals = {},
        builtin = environments.builtin;

    Object.keys(builtin).forEach(function(name) {
        declaredGlobals[name] = builtin[name];
    });

    Object.keys(config.env).forEach(function (name) {
        if (config.env[name]) {
            var environmentGlobals = environments[name] && environments[name].globals;

            // hard to test system environments without globals
            // istanbul ignore else
            if (environmentGlobals) {
                Object.keys(environmentGlobals).forEach(function(name) {
                    declaredGlobals[name] = environmentGlobals[name];
                });
            }
        }
    });

    Object.keys(config.globals).forEach(function(name) {
        declaredGlobals[name] = config.globals[name];
    });

    addGlobalsToScope(declaredGlobals, globalScope);
}

/**
 * Set defautt keys in config object
 * @param  {Object} config Config object
 * @returns {Object}        Normalized value
 */
function normalizeConfig(config) {

    config.globals = config.globals || config.global || {};
    delete config.global;
    return util.mergeConfigs(config, {
        globals: {},
        env: {},
        rules: {}
    });
}

/**
 * Check is value is valid rule config, number or array[number, ...]
 * @param  {Object}  config The rule config
 * @returns {Boolean}        True if valid
 */
function isValidRuleConfig(config) {

    if (typeof config === "number") {
        return config > 0;
    } else if (Array.isArray(config)) {
        // Here the rule looks like [1, ...] - the first value is the key we want
        return config[0] > 0;
    } else {
        return false;
    }
}

/**
 * Init rule by name and config, throws errors if definition is not found or
 * incorrect
 * @param  {Object} api    API instance where rule will be applied
 * @param  {String} name   Name of rule
 * @param  {Object} config Number or object config of rule
 * @returns {Object}        Instance of created rule
 */
function createRule(api, name, config) {
    var ruleCreator = rules.get(name),
        options = [],
        rule;

    if (isValidRuleConfig(config)) {

        if (Array.isArray(config)) {

            // The additional config data is after the bool value
            options = config.slice(1);
        }

        if (ruleCreator) {
            try {
                rule = ruleCreator(new RuleContext(name, api, options));

                // add all the node types as listeners
                Object.keys(rule).forEach(function(nodeType) {
                    api.on(nodeType, rule[nodeType]);
                });
            } catch(ex) {
                ex.message = "Error while loading rule '" + name + "': " + ex.message;
                throw ex;
            }

        } else {
            throw new Error("Definition for rule '" + name + "' was not found.");
        }
    }

    return rule;
}

/**
 * Destroy rule, unregister it from api
 * @param  {Object} api  API instance where rule is applied
 * @param  {Object} rule Created rule
 * @returns {void}
 */
function destroyRule(api, rule) {

    Object.keys(rule).forEach(function(nodeType) {
        api.removeListener(nodeType, rule[nodeType]);
    });
}

/**
 * Create a map of rules by their config
 * @param  {Object} api    API instance
 * @param  {Object} config Map of rules config, name:rule_config
 * @returns {Object}        Map of created rules
 */
function createRules(api, config) {

    var createdRules = {};
    Object.keys(config.rules).forEach(function(key) {
        var rule = createRule(api, key, config.rules[key]);
        if (rule) {
            createdRules[key] = rule;
        }
    });

    return createdRules;
}

/**
 * Create rules by ruleConfigs and merge them with existing rules in API to
 * make work only one instance of same rule
 *
 * @param  {Object} api          API instance
 * @param  {Object} createdRules Already created rules to merge with
 * @param  {Object} ruleConfigs  Map of rules config
 * @returns {void}
 */
function mergeRules(api, createdRules, ruleConfigs) {

    Object.keys(ruleConfigs).forEach(function(name) {

        var ruleConfig = ruleConfigs[name];
        if (createdRules[name]) {
            destroyRule(api, createdRules[name]);
            delete createdRules[name];
        }

        if (ruleConfig) {
            createdRules[name] = createRule(api, name, ruleConfig);
        }
    });
}

function modifyEnv(api, createdRules, globalScope, envItems) {

    Object.keys(envItems).forEach(function (name) {
        if (environments[name]) {
            var environmentRules = environments[name].rules;
            if (environmentRules) {
                mergeRules(api, createdRules, environmentRules);
            }

            var environmentGlobals = environments[name].globals;
            addGlobalsToScope(environmentGlobals, globalScope);
        }
    });
}

/**
 * Parse comment, if it contains specific data, update api
 * @param  {Object} api          API instance
 * @param  {Object} createdRules Map of current rules
 * @param  {Object} globalScope  Global scope of verified code
 * @param  {Object} comment      AST node of comment
 * @returns {void}
 */
function applyConfigComment(api, createdRules, globalScope, comment) {

    var value = comment.value.trim();
    var match = /^(eslint-env|eslint|globals?)\s/.exec(value);

    if (match) {
        value = value.substring(match.index + match[1].length);

        switch (match[1]) {
            case "globals":
            case "global":
                addGlobalsToScope(parseBooleanConfig(value), globalScope);
                break;

            case "eslint-env":
                modifyEnv(api, createdRules, globalScope, parseListConfig(value));
                break;

            case "eslint":
                mergeRules(api, createdRules, parseJsonConfig(value));
                break;

            //no default
        }
    }
}

//------------------------------------------------------------------------------
// Public Interface
//------------------------------------------------------------------------------

/**
 * Object that is responsible for verifying JavaScript text
 * @name eslint
 */
module.exports = (function() {

    var api = Object.create(new EventEmitter()),
        messages = [],
        commentsAttached = false,
        currentText = null,
        currentConfig = null,
        currentTokens = null,
        currentScopes = null,
        currentFilename = null,
        controller = null,
        createdRules = null;


    /**
     * Parses text into an AST. Moved out here because the try-catch prevents
     * optimization of functions, so it's best to keep the try-catch as isolated
     * as possible
     * @param {string} text The text to parse.
     * @returns {ASTNode} The AST if successful or null if not.
     * @private
     */
    function parse(text) {
        /*
         * Check for parsing errors first. If there's a parsing error, nothing
         * else can happen. However, a parsing error does not throw an error
         * from this method - it's just considered a fatal error message, a
         * problem that ESLint identified just like any other.
         */
        try {
            return esprima.parse(text, { loc: true, range: true, raw: true, tokens: true, comment: true });
        } catch (ex) {

            messages.push({
                fatal: true,

                // messages come as "Line X: Unexpected token foo", so strip off leading part
                message: ex.message.substring(ex.message.indexOf(":") + 1).trim(),

                line: ex.lineNumber,
                column: ex.column
            });

            return null;
        }
    }

    // set unlimited listeners (see https://github.com/eslint/eslint/issues/524)
    api.setMaxListeners(0);

    /**
     * Resets the internal state of the object.
     * @returns {void}
     */
    api.reset = function() {
        this.removeAllListeners();
        messages = [];
        commentsAttached = false;
        currentConfig = null;
        currentText = null;
        currentTokens = null;
        currentScopes = null;
        controller = null;
        createdRules = null;
    };

    /**
     * Verifies the text against the rules specified by the second argument.
     * @param {string} text The JavaScript text to verify.
     * @param {Object} config An object whose keys specify the rules to use.
     * @param {string=} filename The optional filename of the file being checked.
     *      If this is not set, the filename will default to '<input>' in the rule context.
     * @param {boolean=} saveState Indicates if the state from the last run should be saved.
     *      Mostly useful for testing purposes.
     * @returns {Object[]} The results as an array of messages or null if no messages.
     */
    api.verify = function(text, config, filename, saveState) {

        var ast;

        // set the current parsed filename
        currentFilename = filename;

        if (!saveState) {
            this.reset();
        }

        ast = parse(text.replace(/^(#![^\r\n]+[\r\n]+)/, "//$1"));

        //if Esprima failed to parse the file, there's no sense in setting up rules
        if (ast) {
            currentText = text;
            controller = new estraverse.Controller();

            // gather data that may be needed by the rules
            currentScopes = escope.analyze(ast, { ignoreEval: true }).scopes;

            /* get all tokens from the ast and store them as a hashtable to
             * improve traversal speed when wanting to find tokens for a given
             * node
             */
            currentTokens = [];
            ast.tokens.forEach(function(token) {
                currentTokens[token.range[0]] = token;
            });

            currentConfig = normalizeConfig(config);

            createdRules = createRules(api, currentConfig);

            // augment global scope with declared global variables
            var rootScope = currentScopes[0];
            addDeclaredGlobals(ast, rootScope, currentConfig);

            /*
             * Each node has a type property. Whenever a particular type of node is found,
             * an event is fired. This allows any listeners to automatically be informed
             * that this type of node has been found and react accordingly.
             */
            controller.traverse(ast, {
                enter: function(node, parent) {
                    var comments = api.getComments(node),
                        leadingComments = comments.leading,
                        trailingComments = comments.trailing;

                    if (leadingComments.length) {

                        leadingComments.forEach(function(node) {
                            if (node.type === "Block") {
                                applyConfigComment(api, createdRules, rootScope, node);
                            }

                            api.emit(node.type + "Comment", node);
                        });
                    }

                    node.parent = parent;

                    api.emit(node.type, node);

                    if (trailingComments.length) {
                        trailingComments.forEach(function(node) {
                            api.emit(node.type + "Comment", node);
                        });
                    }

                },
                leave: function(node) {

                    var comments = api.getComments(node),
                        leadingComments = comments.leading,
                        trailingComments = comments.trailing;

                    if (trailingComments.length) {
                        trailingComments.forEach(function(node) {
                            api.emit(node.type + "Comment:exit", node);
                        });
                    }

                    api.emit(node.type + ":exit", node);

                    if (leadingComments.length) {
                        leadingComments.forEach(function(node) {
                            api.emit(node.type + "Comment:exit", node);
                        });
                    }
                }
            });

        }

        return messages;
    };

    /**
     * Reports a message from one of the rules.
     * @param {string} ruleId The ID of the rule causing the message.
     * @param {ASTNode} node The AST node that the message relates to.
     * @param {Object=} location An object containing the error line and column
     *      numbers. If location is not provided the node's start location will
     *      be used.
     * @param {string} message The actual message.
     * @param {Object} opts Optional template data which produces a formatted message
     *     with symbols being replaced by this object's values.
     * @returns {void}
     */
    api.report = function(ruleId, node, location, message, opts) {

        if (typeof location === "string") {
            opts = message;
            message = location;
            location = node.loc.start;
        }

        Object.keys(opts || {}).forEach(function (key) {
            var rx = new RegExp("{{" + escapeRegExp(key) + "}}", "g");
            message = message.replace(rx, opts[key]);
        });

        messages.push({
            ruleId: ruleId,
            node: node,
            message: message,
            line: location.line,
            column: location.column,
            source: api.getSource(node)
        });
    };

    /**
     * Gets the source code for the given node.
     * @param {ASTNode=} node The AST node to get the text for.
     * @param {int=} beforeCount The number of characters before the node to retrieve.
     * @param {int=} afterCount The number of characters after the node to retrieve.
     * @returns {string} The text representing the AST node.
     */
    api.getSource = function(node, beforeCount, afterCount) {
        if (node) {
            return (currentText !== null) ? currentText.slice(node.range[0] - (beforeCount || 0),
                node.range[1] + (afterCount || 0)) : null;
        } else {
            return currentText;
        }

    };

    /**
     * Gets all comments for the given node.
     * @param {ASTNode} node The AST node to get the comments for.
     * @returns {Object} The list of comments indexed by their position.
     */
    api.getComments = function(node) {
        var ast = controller.root;

        if (!commentsAttached) {
            // Attaching comments is a potentially expensive operation, so we do this lazily.
            estraverse.attachComments(ast, ast.comments, ast.tokens);
            commentsAttached = true;
        }

        return {
            leading: node.leadingComments || [],
            trailing: node.trailingComments || []
        };
    };

    /**
     * Retrieves the JSDoc comment for a given node.
     * @param {ASTNode} node The AST node to get the comment for.
     * @returns {ASTNode} The BlockComment node containing the JSDoc for the
     *      given node or null if not found.
     */
    api.getJSDocComment = function(node) {

        var parent = node.parent,
            line = node.loc.start.line;

        /**
         * Finds a JSDoc comment node in an array of comment nodes.
         * @param {ASTNode[]} comments The array of comment nodes to search.
         * @returns {ASTNode} The node if found, null if not.
         * @private
         */
        function findJSDocComment(comments) {

            if (comments) {
                for (var i = comments.length - 1; i >= 0; i--) {
                    if (comments[i].type === "Block" && comments[i].value.charAt(0) === "*") {

                        if (line - comments[i].loc.end.line <= 1) {
                            return comments[i];
                        } else {
                            break;
                        }
                    }
                }
            }

            return null;
        }

        switch(node.type) {
            case "FunctionDeclaration":

                // first global function has its comments stolen by Program
                var nodeToCheck = (node.leadingComments ? node : parent);
                return findJSDocComment(nodeToCheck.leadingComments);

            case "FunctionExpression":

                if (parent.type !== "CallExpression" || parent.callee !== node) {
                    while (parent && !parent.leadingComments && parent.type !== "FunctionExpression" && parent.type !== "FunctionDeclaration") {
                        parent = parent.parent;
                    }

                    return parent ? findJSDocComment(parent.leadingComments) : null;
                }

                // falls through

            default:
                return null;
        }
    };

    /**
     * Gets a number of tokens that precede a given node's tokens in the token stream.
     * @param {ASTNode} node The AST node.
     * @param {int} [beforeCount=0] The number of tokens before the node to retrieve.
     * @returns {[Token]} Array of objects representing tokens.
     */
    api.getTokensBefore = function(node, beforeCount) {
        var beforeTokens = [], cursor = node.range[0] - 1;
        while (beforeCount > 0 && cursor >= 0) {
            if(currentTokens[cursor]) {
                beforeTokens.unshift(currentTokens[cursor]);
                --beforeCount;
            }
            --cursor;
        }
        return beforeTokens;
    };

    /**
     * Gets the token that precedes a given node's tokens in the token stream.
     * @param {ASTNode} node The AST node.
     * @param {int} [skip=0] A number of tokens to skip before the given node.
     * @returns {Token} An object representing the token.
     */
    api.getTokenBefore = function(node, skip) {
        for (var cursor = node.range[0] - 1; cursor >= 0; --cursor) {
            if (currentTokens[cursor]) {
                if (skip > 0) {
                    --skip;
                } else {
                    return currentTokens[cursor];
                }
            }
        }
    };

    /**
     * Gets a number of tokens that precede a given node's tokens in the token stream.
     * @param {ASTNode} node The AST node.
     * @param {int} [afterCount=0] The number of tokens after the node to retrieve.
     * @returns {[Token]} Array of objects representing tokens.
     */
    api.getTokensAfter = function(node, afterCount) {
        var afterTokens = [], cursor = node.range[1];
        while (afterCount > 0 && cursor < currentTokens.length) {
            if(currentTokens[cursor]) {
                afterTokens.push(currentTokens[cursor]);
                --afterCount;
                cursor = currentTokens[cursor].range[1];
            } else {
                ++cursor;
            }
        }
        return afterTokens;
    };

    /**
     * Gets the token that follows a given node's tokens in the token stream.
     * @param {ASTNode} node The AST node.
     * @param {int} [skip=0] A number of tokens to skip after the given node.
     * @returns {Token} An object representing the token.
     */
    api.getTokenAfter = function(node, skip) {
        for (var cursor = node.range[1]; cursor < currentTokens.length; ++cursor) {
            if (currentTokens[cursor]) {
                if (skip > 0) {
                    --skip;
                } else {
                    return currentTokens[cursor];
                }
            }
        }
    };

    /**
     * Gets all tokens that are related to the given node.
     * @param {ASTNode} node The AST node.
     * @param {int} [beforeCount=0] The number of tokens before the node to retrieve.
     * @param {int} [afterCount=0] The number of tokens after the node to retrieve.
     * @returns {[Token]} Array of objects representing tokens.
     */
    api.getTokens = function(node, beforeCount, afterCount) {
        var beforeTokens = api.getTokensBefore(node, beforeCount),
            afterTokens = api.getTokensAfter(node, afterCount),
            tokens = [],
            cursor = node.range[0];
        while (cursor < node.range[1]) {
            if(currentTokens[cursor]) {
                tokens.push(currentTokens[cursor]);
                cursor = currentTokens[cursor].range[1];
            } else {
                ++cursor;
            }
        }
        return beforeTokens.concat(tokens, afterTokens);
    };

    /**
     * Gets the first `count` tokens of the given node's token stream.
     * @param {ASTNode} node The AST node.
     * @param {int} [count=0] The number of tokens of the node to retrieve.
     * @returns {[Token]} Array of objects representing tokens.
     */
    api.getFirstTokens = function(node, count) {
        var tokens = [], cursor = node.range[0];
        while (count > 0 && cursor < node.range[1]) {
            if(currentTokens[cursor]) {
                tokens.push(currentTokens[cursor]);
                --count;
                cursor = currentTokens[cursor].range[1];
            } else {
                ++cursor;
            }
        }
        return tokens;
    };

    /**
     * Gets the first token of the given node's token stream.
     * @param {ASTNode} node The AST node.
     * @param {int} [skip=0] A number of tokens to skip.
     * @returns {Token} An object representing the token.
     */
    api.getFirstToken = function(node, skip) {
        for (var cursor = node.range[0]; cursor < node.range[1]; ++cursor) {
            if (currentTokens[cursor]) {
                if (skip > 0) {
                    --skip;
                } else {
                    return currentTokens[cursor];
                }
            }
        }
    };

    /**
     * Gets the last `count` tokens of the given node.
     * @param {ASTNode} node The AST node.
     * @param {int} [count=0] The number of tokens of the node to retrieve.
     * @returns {[Token]} Array of objects representing tokens.
     */
    api.getLastTokens = function(node, count) {
        var tokens = [], cursor = node.range[1] - 1;
        while (count > 0 && cursor >= node.range[0]) {
            if(currentTokens[cursor]) {
                tokens.unshift(currentTokens[cursor]);
                --count;
            }
            --cursor;
        }
        return tokens;
    };

    /**
     * Gets the last token of the given node's token stream.
     * @param {ASTNode} node The AST node.
     * @param {int} [skip=0] A number of tokens to skip.
     * @returns {Token} An object representing the token.
     */
    api.getLastToken = function(node, skip) {
        for (var cursor = node.range[1] - 1; cursor >= node.range[0]; --cursor) {
            if (currentTokens[cursor]) {
                if (skip > 0) {
                    --skip;
                } else {
                    return currentTokens[cursor];
                }
            }
        }
    };

    /**
     * Gets nodes that are ancestors of current node.
     * @returns {ASTNode[]} Array of objects representing ancestors.
     */
    api.getAncestors = function() {
        return controller.parents();
    };


    /**
     * Gets the scope for the current node.
     * @returns {Object} An object representing the current node's scope.
     */
    api.getScope = function() {
        var parents = controller.parents().reverse(),
            innerBlock = null;

        // Don't do this for Program nodes - they have no parents
        if (parents.length) {

            // if current node is function declaration, add it to the list
            var current = controller.current();
            if (current.type === "FunctionDeclaration" || current.type === "FunctionExpression") {
                parents.splice(0, 0, current);
            }

            // Ascend the current node's parents
            for (var i = 0; i < parents.length; i++) {

                // The first node that requires a scope is the node that will be
                // our current node's innermost scope.
                if (escope.Scope.isScopeRequired(parents[i])) {
                    innerBlock = parents[i];
                    break;
                }
            }

            // Loop through the scopes returned by escope to find the innermost
            // scope and return that scope.
            for (var j = 0; j < currentScopes.length; j++) {
                if (innerBlock.type === currentScopes[j].block.type &&
                    innerBlock.range[0] === currentScopes[j].block.range[0] &&
                    innerBlock.range[1] === currentScopes[j].block.range[1]) {

                    // Escope returns two similar scopes for named functional
                    // expression, we should take the last
                    if ((innerBlock.type === "FunctionExpression" && innerBlock.id && innerBlock.id.name)) {

                        var nextScope = currentScopes[j + 1];
                        return nextScope;
                    }

                    return currentScopes[j];
                }
            }
        } else {
            return currentScopes[0];    // global scope
        }
    };

    /**
     * Determines if a given node matches a given selector.
     * @param {ASTNode} node An AST node.
     * @param {string} selector The CSS Auron query selector to match.
     * @returns {boolean} True if the node matches, false if not.
     */
    api.matches = function(node, selector) {
        return !!cssauron(selector)(node);
    };

    /**
     * Gets the filename for the currently parsed source.
     * @returns {string} The filename associated with the source being parsed.
     *     Defaults to "<input>" if no filename info is present.
     */
    api.getFilename = function() {
        if (typeof currentFilename === "string") {
            return currentFilename;
        } else {
            return "<input>";
        }
    };

    /**
     * Defines a new linting rule.
     * @param {string} ruleId A unique rule identifier
     * @param {Function} ruleModule Function from context to object mapping AST node types to event handlers
     * @returns {void}
     */
    var defineRule = api.defineRule = function(ruleId, ruleModule) {
        rules.define(ruleId, ruleModule);
    };

    /**
     * Defines many new linting rules.
     * @param {object} rules map from unique rule identifier to rule
     * @returns {void}
     */
    api.defineRules = function(rules) {
        Object.getOwnPropertyNames(rules).forEach(function(ruleId){
            defineRule(ruleId, rules[ruleId]);
        });
    };

    /**
     * Gets the default eslint configuration.
     * @returns {Object} Object mapping rule IDs to their default configurations
     */
    api.defaults = function() {
        return require("../conf/eslint.json");
    };

    return api;

}());
