import React from 'react';
import { BrowserRouter, Route, Routes } from "react-router-dom";
import { URLS } from "./constant/urls"
import withLayout from "./component/Layout/withLayout";
import Graphs from "./component/Graphs/Graphs";

function App() {
    const UsersWithLayout = withLayout(Graphs);
    return (
        <BrowserRouter>
            <Routes>
                <Route path={URLS.home} element={<UsersWithLayout/>} />
            </Routes>
        </BrowserRouter>
    );
}

export default App;
