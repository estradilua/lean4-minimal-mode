import type { InfoviewApi } from '@leanprover/infoview-api';
import { RPCEditorApi } from './api.ts';
import './es-module-shims-options.js';
import 'es-module-shims';

import './index.css';

const host = `http://${location.host}/imports`;

const imports = {
  '@leanprover/infoview': `${host}/index.production.min.js`,
  react: `${host}/react.production.min.js`,
  'react/jsx-runtime': `${host}/react-jsx-runtime.production.min.js`,
  'react-dom': `${host}/react-dom.production.min.js`,
};

const rootEl = document.querySelector('#root');

const editorApi = new RPCEditorApi();
importShim.addImportMap({ imports });

importShim('@leanprover/infoview').then((mod: any) => {
  const api: InfoviewApi = mod.renderInfoview(editorApi, rootEl);
  editorApi.initPanel(api);
});
