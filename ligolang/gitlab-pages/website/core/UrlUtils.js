exports.docUrl = (config, doc, language) => {
  const docsPart = config.docsUrl ? `${config.docsUrl}/` : '';
  const langPart = language ? `${language}/` : '';

  return `${config.baseUrl}${docsPart}${langPart}${doc}`;
};

exports.pageUrl = (config, doc, language) => {
  const langPart = language ? `${language}/` : '';
  return `${config.baseUrl}${langPart}${doc}`;
};
