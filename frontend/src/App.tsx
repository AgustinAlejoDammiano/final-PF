import React from 'react';
import { BrowserRouter, Route, Routes } from "react-router-dom";
import { URLS } from "./constant/urls"
import withLayout from "./component/Layout/withLayout";
import Graphs from "./component/Graphs/Graphs";
import Jurisdiction from "./component/JurisdictionList/JurisdictionList";

function App() {
    const GraphsWithLayout = withLayout(Graphs);
    const JurisdictionWithLayout = withLayout(Jurisdiction);
    return (
        <BrowserRouter>
            <Routes>
                <Route path={URLS.home} element={<GraphsWithLayout/>} />
                <Route path={URLS.jurisdiction} element={<JurisdictionWithLayout/>} />
            </Routes>
        </BrowserRouter>
    );
}

export default App;
