
const repoUrl = 'https://gitlab.com/ligolang/ligo';
const versions = require('./versions.json');

// let reasonHighlightJs = require('reason-highlightjs');

const siteConfig = {
  title: 'LIGO', // Title for your website.
  tagline: 'LIGO is a friendly smart contract language for Tezos',
  // taglineSub: 'Michelson was never so easy',
  url: 'https://ligolang.org', // Your website URL
  baseUrl: '/', // Base URL for your project */
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  // Used for publishing and more
  projectName: 'ligo',
  organizationName: 'TBN',
  // For top-level user or org sites, the organization is still the same.
  // e.g., for the https://JoelMarcey.github.io site, it would be set like...
  //   organizationName: 'JoelMarcey'

  // For no header links in the top nav bar -> headerLinks: [],
  

  // footerLinks: {
  //   docs: [
  //     { doc: 'intro/installation', label: 'Install' },
  //     { doc: 'api/cli-commands', label: 'CLI Commands' },
  //     { doc: 'contributors/origin', label: 'Contribute' },
  //     { href: '/odoc', label: 'API Documentation' }
  //   ],
  //   community: [
  //     {
  //       href: 'https://forum.tezosagora.org/tag/ligo',
  //       label: 'Tezos Agora Forum',
  //       blankTarget: true
  //     },
  //     {
  //       href: 'https://tezos.stackexchange.com/questions/tagged/ligo',
  //       label: 'Tezos Stack Exchange',
  //       blankTarget: true
  //     },
  //     {
  //       href: 'https://t.me/LigoLang',
  //       label: 'Telegram',
  //       blankTarget: true
  //     },
  //     {
  //       href: 'https://discord.gg/9rhYaEt',
  //       label: 'Discord',
  //       blankTarget: true
  //     }
  //   ],
  //   more: [
  //     {
  //       doc: 'tutorials/get-started/tezos-taco-shop-smart-contract',
  //       label: 'Tutorials'
  //     },
  //     { href: repoUrl, label: 'GitLab' }
  //   ]
  // },

  favicon: 'img/circle.svg',

  /* highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: 'default',
    hljs: function (hljs) {
      hljs.registerLanguage('reasonligo', reasonHighlightJs);
      hljs.registerLanguage('pascaligo', function (hljs) {
        return {
          // case_insensitive: true,
          beginKeywords: '',
          keywords: {
            keyword:
            'and attributes begin big_map block case const contains else'
              + ' end False for from function if in is list map mod nil'
              + ' not of or patch record remove set skip then to True type'
              + ' var while with',
            literal: 'true false unit int string Some None bool nat list'
          },
          lexemes: '[a-zA-Z][a-zA-Z0-9_]*',
          contains: [
            hljs.C_LINE_COMMENT_MODE,

            {
              className: 'type',
              begin: /[A-Z][a-z]+/
            },
            {
              begin: /[*+-:;\(\)\{\}|\>\<]/
              // className: 'ignore'
            }
          ]
        };
      });
    }
  },*/

  // Add custom scripts here that would be placed in <script> tags.

  // On page navigation for the current documentation page.
  // No .html extensions for paths.
  
  // Show documentation's last contributor's name.
  // enableUpdateBy: true,

  // Show documentation's last update time.
  // enableUpdateTime: true,

  // You may provide arbitrary config keys to be used as needed by your
  // template. For example, if you need your repo's URL...
  // repoUrl: repoUrl,
  plugins: [
    require.resolve('@ligo/syntax', {

    },
    '@docusaurus/plugin-sitemap', {
      cacheTime: 600 * 1000, // 600 sec - cache purge period
      changefreq: 'weekly',
      priority: 0.5,
    }),
    [
      '@docusaurus/plugin-client-redirects', {        
        redirects: [          
          { from: ["/docs/next/manpages/compile-contract"     ], to: "/docs/next/manpages/compile contract"       },
          { from: ["/docs/next/manpages/compile-expression"   ], to: "/docs/next/manpages/compile expression"     },
          { from: ["/docs/next/manpages/compile-parameter"    ], to: "/docs/next/manpages/compile parameter"      },
          { from: ["/docs/next/manpages/compile-storage"      ], to: "/docs/next/manpages/compile storage"        },
          { from: ["/docs/next/manpages/dry-run"              ], to: "/docs/next/manpages/run dry-run"            },
          { from: ["/docs/next/manpages/evaluate-expr"        ], to: "/docs/next/manpages/run evaluate-expr"      },
          { from: ["/docs/next/manpages/get-scope"            ], to: "/docs/next/manpages/info get-scope"         },
          { from: ["/docs/next/manpages/interpret"            ], to: "/docs/next/manpages/run interpret"          },
          { from: ["/docs/next/manpages/list-declarations"    ], to: "/docs/next/manpages/info list-declarations" },
          { from: ["/docs/next/manpages/measure-contract"     ], to: "/docs/next/manpages/info measure-contract"  },
          { from: ["/docs/next/manpages/preprocess"           ], to: "/docs/next/manpages/print preprocessed"     },
          { from: ["/docs/next/manpages/pretty-print"         ], to: "/docs/next/manpages/print pretty-print"     },
          { from: ["/docs/next/manpages/print-ast"            ], to: "/docs/next/manpages/print ast"              },
          { from: ["/docs/next/manpages/print-ast-combined"   ], to: "/docs/next/manpages/print ast-combined"     },
          { from: ["/docs/next/manpages/print-ast-core"       ], to: "/docs/next/manpages/print ast-core"         },
          { from: ["/docs/next/manpages/print-ast-sugar"      ], to: "/docs/next/manpages/print ast-sugar"        },
          { from: ["/docs/next/manpages/print-ast-typed"      ], to: "/docs/next/manpages/print ast-typed"        },
          { from: ["/docs/next/manpages/print-cst"            ], to: "/docs/next/manpages/print cst"              },
          { from: ["/docs/next/manpages/print-graph"          ], to: "/docs/next/manpages/print dependency-graph" },
          { from: ["/docs/next/manpages/print-mini-c"         ], to: "/docs/next/manpages/print mini-c"           },
          { from: ["/docs/next/manpages/evaluate-call"        ], to: "/docs/next/manpages/run evaluate-call"      },
          { from: ["/docs/next/manpages/test"                 ], to: "/docs/next/manpages/run test"               },
          { from: ["/docs/next/manpages/transpile-contract"   ], to: "/docs/next/manpages/transpile contract"     },
          { from: ["/docs/next/manpages/transpile-expression" ], to: "/docs/next/manpages/transpile expression"   },
          { from: ["/docs/next/language-basics/strings"       ], to: "/docs/next/language-basics/strings-bytes"        },
        ],      
      }
    ]
  ],
  
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          // docs folder path relative to website dir.
          path: '../docs',
          // sidebars file relative to website dir.
          sidebarPath: require.resolve('./sidebars.json'),
        },
        theme: {
          customCss: require.resolve('./static/css/custom.css'),
        },
        versions: {
          current: {
            label: `(unreleased)`,
          },
        
        }
      }
    ]
  ],
  themeConfig: {
    googleAnalytics: {
      trackingID: 'UA-153751765-1',
      gtag: true
    },
    algolia: {
      apiKey: '12be98d9fd4242a5f16b70a5cc6b0158',
      indexName: 'ligolang',
      contextualSearch: true,
      // searchParameters: {
      //   facetFilters: ["version:0.7.1"]
      // },
      // algoliaOptions: {} // Optional, if provided by Algolia
    },
    navbar: {
      logo: {
        alt: 'LIGO Logo',
        src: 'img/logo.svg',
        srcDark: 'img/logo-night.svg'
      },
      items: [
        { type: 'docsVersionDropdown', position: 'left'},
        { href: 'https://ide.ligolang.org/', label: 'Try Online', position: 'left', target: '_self' },
        { to: 'docs/intro/installation', label: 'Install', position: 'left' },
        { to: 'docs/intro/introduction', label: 'Docs', position: 'left' },
        {
          to: 'docs/tutorials/get-started/tezos-taco-shop-smart-contract',
          label: 'Tutorials',
          position: 'left'
        },
        { href: 'https://forum.tezosagora.org/tag/ligo', label: 'Blog', position: 'left' },
        // TODO: { href: "/odoc", label: "API" },
        // { doc: 'contributors/origin', label: 'Contribute' },
        { to: '/contact', label: 'Ask Questions', position: 'left' }
      ],
    },
    footer: {
      links: [ 
        {
          title: 'Docs', 
          items: [
            { to: 'docs/intro/installation', label: 'Install' },
            { to: 'docs/api/cli-commands', label: 'CLI Commands' },
            { to: 'docs/api/cheat-sheet', label: 'Cheat Sheet' },
            { href: 'https://ligolang.org/odoc/', label: 'API Documentation' }
          ]
        },
        {
          title: 'Community',
          items: [
            {
              href: 'https://tezos.stackexchange.com/questions/tagged/ligo',
              label: 'Tezos Stack Exchange'
            },
            {
              href: 'https://discord.gg/9rhYaEt',
              label: 'Discord'
            },
            {
              href: 'https://t.me/LigoLang',
              label: 'Telegram'
            },
            {
              href: 'https://riot.im/app/#/room/#ligo-public:matrix.org',
              label: 'Riot'
            }
          ]
        },
        {
          title: 'More',
          items: [
            {
              label: 'Tutorials',
              to: 'docs/tutorials/get-started/tezos-taco-shop-smart-contract'
            },
            {
              href: 'https://forum.tezosagora.org/tag/ligo',
              label: 'Blog'
            },
            {
              label: 'GitLab',
              href: repoUrl
            },
            {
              label: 'Contribute',
              to: 'docs/contributors/origin'
            }
          ]
        }
        
        // { href: 'https://ide.ligolang.org/', title: 'Try Online' }
      ],
      copyright: `© ${new Date().getFullYear()} LIGO. All rights reserved.`,
    },
    image: 'img/docusaurus.png',
    sidebarCollapsible: true,
    prism: {
      theme: require('prism-react-renderer/themes/github'),
      darkTheme: require('prism-react-renderer/themes/vsDark')
    },
  }
};

module.exports = siteConfig;
