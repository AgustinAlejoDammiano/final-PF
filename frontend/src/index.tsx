import React, { Suspense } from "react";
import ReactDOM from "react-dom";
// import { Provider } from "react-redux";
// import store from "@entrypoint/presenters/web/store/store";
import { ThemeProvider } from "@material-ui/styles";
import { CssBaseline } from "@material-ui/core";
import Themes from "./themes";
import App from "./App";

ReactDOM.render(
  <Suspense fallback={(<div>{"loading"}</div>)}>
        {/* <Provider store={store}> */}
            <ThemeProvider theme={Themes}>
                <CssBaseline />
                <App />
            </ThemeProvider>
        {/* </Provider> */}
  </Suspense>,
  document.getElementById("root"),
);
