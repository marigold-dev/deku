// deprecated.

const React = require('react');
const docUrl = require(`${process.cwd()}/core/UrlUtils`).docUrl;

const Link = props => {
  const link = props.href ? props.href : docUrl(props.config, props.doc);

  return props.blankTarget ? (
    <a href={link} target="_blank" rel="noreferrer noopener">
      {props.label}
    </a>
  ) : (
    <a href={link}>{props.label}</a>
  );
};

module.exports = props => {
  return (
    <footer className="nav-footer" id="footer">
      <section className="footer-wrapper">
        <div className="sitemap">
          <div>
            <h5>Docs</h5>
            {props.config.footerLinks.docs.map(entry =>
              Link({ config: props.config, ...entry })
            )}
          </div>
          <div>
            <h5>Community</h5>
            {props.config.footerLinks.community.map(entry =>
              Link({ config: props.config, ...entry })
            )}
          </div>
          <div>
            <h5>More</h5>
            {props.config.footerLinks.more.map(entry =>
              Link({ config: props.config, ...entry })
            )}
          </div>
        </div>

        <div className="copyright">{props.config.copyright}</div>
      </section>
    </footer>
  );
};
