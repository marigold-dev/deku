const React = require("react");

module.exports = props => {
  const docsVersion = props.version;
  const algoliaOptions = props.config.algolia.algoliaOptions
    ? props.config.algolia.algoliaOptions
    : {};

  return (
    <div className="algoliaSearch">
      <img className="icon" src="img/search_icon.svg" />
      <input id="searchDocs" type="text" placeholder="Search" title="Search" />
      {props.config.algolia && (
        <div>
          <script
            type="text/javascript"
            src="https://cdn.jsdelivr.net/docsearch.js/1/docsearch.min.js"
          ></script>
          <script
            dangerouslySetInnerHTML={{
              __html: `
              docsearch({
                ${
                  props.config.algolia.appId
                    ? `appId: '${props.config.algolia.appId}',`
                    : ""
                }
                apiKey: '${props.config.algolia.apiKey}',
                indexName: '${props.config.algolia.indexName}',
                inputSelector: '#searchDocs',
                algoliaOptions: ${JSON.stringify(algoliaOptions)
                  .replace("VERSION", docsVersion)
                  .replace("LANGUAGE", props.language)}
              });
            `
            }}
          />
        </div>
      )}
    </div>
  );
};
