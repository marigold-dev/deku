const React = require('react');
const AlgoliaSearch = require(`${process.cwd()}/core/AlgoliaSearch`);

module.exports = props => {
  return (
    <div id="pageNotFoundPage" className="centered">
      <div id="mural">
        <img
          className="muralPolygon1"
          src={`${props.config.baseUrl}img/404-mural/polygon1.svg`}
        />
        <img
          className="muralPolygon2"
          src={`${props.config.baseUrl}img/404-mural/polygon2.svg`}
        />
        <img
          className="muralPolygon3"
          src={`${props.config.baseUrl}img/404-mural/polygon3.svg`}
        />
        <img
          className="muralPolygon4"
          src={`${props.config.baseUrl}img/404-mural/polygon4.svg`}
        />
        <img
          className="muralPolygon5"
          src={`${props.config.baseUrl}img/404-mural/polygon5.svg`}
        />
      </div>
      <div id="message">
        <div className="title">404</div>
        <div className="lookingForSomething">
          You're looking for something that doesn't exist
        </div>
        <div className="discoveriesHappen">
          That's how great discoveries happen!
        </div>
        <div className="letUsHelpWrapper">
          <div className="letUsHelp">Let us help</div>
          <AlgoliaSearch {...props}></AlgoliaSearch>
        </div>
      </div>
    </div>
  );
};
