/**
   PoC esy plugin for esybuild.
   Lightly tweaks the original https://github.com/yarnpkg/berry/blob/97904dfb0fef7b8b1a82be5211bb7a8a31c895e8/packages/esbuild-plugin-pnp/sources/index.ts.
   */
"use strict";
var __assign =
  (this && this.__assign) ||
  function () {
    __assign =
      Object.assign ||
      function (t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
          s = arguments[i];
          for (var p in s)
            if (Object.prototype.hasOwnProperty.call(s, p)) t[p] = s[p];
        }
        return t;
      };
    return __assign.apply(this, arguments);
  };
var __awaiter =
  (this && this.__awaiter) ||
  function (thisArg, _arguments, P, generator) {
    function adopt(value) {
      return value instanceof P
        ? value
        : new P(function (resolve) {
            resolve(value);
          });
    }
    return new (P || (P = Promise))(function (resolve, reject) {
      function fulfilled(value) {
        try {
          step(generator.next(value));
        } catch (e) {
          reject(e);
        }
      }
      function rejected(value) {
        try {
          step(generator["throw"](value));
        } catch (e) {
          reject(e);
        }
      }
      function step(result) {
        result.done
          ? resolve(result.value)
          : adopt(result.value).then(fulfilled, rejected);
      }
      step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
  };
var __generator =
  (this && this.__generator) ||
  function (thisArg, body) {
    var _ = {
        label: 0,
        sent: function () {
          if (t[0] & 1) throw t[1];
          return t[1];
        },
        trys: [],
        ops: [],
      },
      f,
      y,
      t,
      g;
    return (
      (g = { next: verb(0), throw: verb(1), return: verb(2) }),
      typeof Symbol === "function" &&
        (g[Symbol.iterator] = function () {
          return this;
        }),
      g
    );
    function verb(n) {
      return function (v) {
        return step([n, v]);
      };
    }
    function step(op) {
      if (f) throw new TypeError("Generator is already executing.");
      while (_)
        try {
          if (
            ((f = 1),
            y &&
              (t =
                op[0] & 2
                  ? y["return"]
                  : op[0]
                  ? y["throw"] || ((t = y["return"]) && t.call(y), 0)
                  : y.next) &&
              !(t = t.call(y, op[1])).done)
          )
            return t;
          if (((y = 0), t)) op = [op[0] & 2, t.value];
          switch (op[0]) {
            case 0:
            case 1:
              t = op;
              break;
            case 4:
              _.label++;
              return { value: op[1], done: false };
            case 5:
              _.label++;
              y = op[1];
              op = [0];
              continue;
            case 7:
              op = _.ops.pop();
              _.trys.pop();
              continue;
            default:
              if (
                !((t = _.trys), (t = t.length > 0 && t[t.length - 1])) &&
                (op[0] === 6 || op[0] === 2)
              ) {
                _ = 0;
                continue;
              }
              if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) {
                _.label = op[1];
                break;
              }
              if (op[0] === 6 && _.label < t[1]) {
                _.label = t[1];
                t = op;
                break;
              }
              if (t && _.label < t[2]) {
                _.label = t[2];
                _.ops.push(op);
                break;
              }
              if (t[2]) _.ops.pop();
              _.trys.pop();
              continue;
          }
          op = body.call(thisArg, _);
        } catch (e) {
          op = [6, e];
          y = 0;
        } finally {
          f = t = 0;
        }
      if (op[0] & 5) throw op[1];
      return { value: op[0] ? op[1] : void 0, done: true };
    }
  };
exports.__esModule = true;
exports.pnpPlugin = void 0;
var fs = require("fs");
var pathM = require("path");
var cp = require("child_process");
var matchAll = /()/;
var defaultExtensions = [
  ".tsx",
  ".ts",
  ".jsx",
  ".mjs",
  ".cjs",
  ".js",
  ".css",
  ".json",
];
// Reference: https://github.com/evanw/esbuild/blob/537195ae84bee1510fac14235906d588084c39cd/pkg/api/api_impl.go#L366-L388
function parseExternals(externals) {
  return externals.map(function (external) {
    // ESBuild's validation pass runs before this function is called so there's no need to assert that there's a single wildcard
    var wildcardIdx = external.indexOf("*");
    if (wildcardIdx !== -1)
      return {
        prefix: external.slice(0, wildcardIdx),
        suffix: external.slice(wildcardIdx + 1),
      };
    return external;
  });
}

function isExternal(path, externals) {
  for (var _i = 0, externals_1 = externals; _i < externals_1.length; _i++) {
    var external_1 = externals_1[_i];
    if (typeof external_1 === "object") {
      // Reference: https://github.com/evanw/esbuild/blob/537195ae84bee1510fac14235906d588084c39cd/internal/resolver/resolver.go#L372-L381
      if (
        path.length >= external_1.prefix.length + external_1.suffix.length &&
        path.startsWith(external_1.prefix) &&
        path.endsWith(external_1.suffix)
      ) {
        return true;
      }
    } else {
      if (path === external_1) return true;
      // If the module "foo" has been marked as external, we also want to treat
      // paths into that module such as "foo/bar" as external too.
      // References:
      // - https://github.com/evanw/esbuild/blob/537195ae84bee1510fac14235906d588084c39cd/internal/resolver/resolver.go#L651-L652
      // - https://github.com/evanw/esbuild/blob/537195ae84bee1510fac14235906d588084c39cd/internal/resolver/resolver.go#L1688-L1691
      if (
        !external_1.startsWith("/") &&
        !external_1.startsWith("./") &&
        !external_1.startsWith("../") &&
        external_1 !== "." &&
        external_1 !== ".."
      ) {
        if (path.startsWith(external_1 + "/")) {
          return true;
        }
      }
    }
  }
  return false;
}

function defaultOnLoad(args) {
  return __awaiter(this, void 0, void 0, function () {
    var _a;
    return __generator(this, function (_b) {
      switch (_b.label) {
        case 0:
          _a = {};
          return [4 /*yield*/, fs.promises.readFile(args.path)];
        case 1:
          return [
            2 /*return*/,
            ((_a.contents = _b.sent()), (_a.loader = "default"), _a),
          ];
      }
    });
  });
}
function defaultOnResolve(args, _a) {
  var resolvedPath = _a.resolvedPath,
    error = _a.error,
    watchFiles = _a.watchFiles;
  return __awaiter(this, void 0, void 0, function () {
    var problems, mergeWith;
    return __generator(this, function (_b) {
      problems = error ? [{ text: error.message }] : [];
      switch (args.kind) {
        case "require-call":
        case "require-resolve":
        case "dynamic-import":
          {
            mergeWith = { warnings: problems };
          }
          break;
        default:
          {
            mergeWith = { errors: problems };
          }
          break;
      }
      if (resolvedPath !== null) {
        return [
          2 /*return*/,
          { namespace: "pnp", path: resolvedPath, watchFiles: watchFiles },
        ];
      } else {
        return [
          2 /*return*/,
          __assign(__assign({ external: true }, mergeWith), {
            watchFiles: watchFiles,
          }),
        ];
      }
      return [2 /*return*/];
    });
  });
}

let curRoot = process.env["cur__root"];
var pnpApi = require((curRoot
  ? curRoot
  : pathM.dirname(
      JSON.parse(cp.execSync("esy status").toString()).rootPackageConfigPath
    )) + "/_esy/default/pnp.js");

function pnpPlugin(_a) {
  var _b = _a === void 0 ? {} : _a,
    _c = _b.baseDir,
    baseDir = _c === void 0 ? process.cwd() : _c,
    _d = _b.extensions,
    extensions = _d === void 0 ? defaultExtensions : _d,
    _e = _b.filter,
    filter = _e === void 0 ? matchAll : _e,
    _f = _b.onResolve,
    onResolve = _f === void 0 ? defaultOnResolve : _f,
    _g = _b.onLoad,
    onLoad = _g === void 0 ? defaultOnLoad : _g;
  return {
    name: "esbuild-plugin-esy",
    setup: function (build) {
      var _a, _b;
      var externals = parseExternals(
        (_a = build.initialOptions.external) !== null && _a !== void 0 ? _a : []
      );
      var platform =
        (_b = build.initialOptions.platform) !== null && _b !== void 0
          ? _b
          : "browser";
      var isPlatformNode = platform === "node";
      // Reference: https://github.com/evanw/esbuild/blob/537195ae84bee1510fac14235906d588084c39cd/internal/resolver/resolver.go#L238-L253
      var conditionsDefault = new Set(build.initialOptions.conditions);
      conditionsDefault.add("default");
      if (platform === "browser" || platform === "node")
        conditionsDefault.add(platform);
      var conditionsImport = new Set(conditionsDefault);
      conditionsImport.add("import");
      var conditionsRequire = new Set(conditionsDefault);
      conditionsRequire.add("require");
      build.onResolve({ filter: filter }, function (args) {
        var _a, _b;
        if (isExternal(args.path, externals)) return { external: true };
        // Reference: https://github.com/evanw/esbuild/blob/537195ae84bee1510fac14235906d588084c39cd/internal/resolver/resolver.go#L1495-L1502
        var conditions = conditionsDefault;
        if (args.kind === "dynamic-import" || args.kind === "import-statement")
          conditions = conditionsImport;
        else if (
          args.kind === "require-call" ||
          args.kind === "require-resolve"
        )
          conditions = conditionsRequire;
        // The entry point resolution uses an empty string
        var effectiveImporter = args.importer ? args.importer : baseDir + "/";

        if (!pnpApi)
          // Path isn't controlled by PnP so delegate to the next resolver in the chain
          return undefined;
        var path = null;
        var error;
        try {
          path = pnpApi.resolveRequest(args.path, effectiveImporter, {
            conditions: conditions,
            considerBuiltins: isPlatformNode,
            extensions: extensions,
          });
        } catch (e) {
          error = e;
        }
        var watchFiles = [pnpApi.resolveRequest("pnpapi", null)];
        if (path) {
          var locator = pnpApi.findPackageLocator(path);
          if (locator) {
            var info = pnpApi.getPackageInformation(locator);
            if (
              (info === null || info === void 0 ? void 0 : info.linkType) ===
              "SOFT"
            ) {
              watchFiles.push(
                (_b =
                  (_a = pnpApi.resolveVirtual) === null || _a === void 0
                    ? void 0
                    : _a.call(pnpApi, path)) !== null && _b !== void 0
                  ? _b
                  : path
              );
            }
          }
        }
        return onResolve(args, {
          resolvedPath: path,
          error: error,
          watchFiles: watchFiles,
        });
      });
      // We register on the build to prevent ESBuild from reading the files
      // itself, since it wouldn't know how to access the files from within
      // the zip archives.
      if (build.onLoad !== null) {
        build.onLoad({ filter: filter }, onLoad);
      }
    },
  };
}
exports.pnpPlugin = pnpPlugin;
