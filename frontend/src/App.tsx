import React from 'react';
import { BrowserRouter, Route, Routes } from "react-router-dom";
import { URLS } from "./constant/urls"
import withLayout from "./component/Layout/withLayout";
import Graphs from "./component/Graphs/Graphs";
import Jurisdiction from "./component/JurisdictionList/JurisdictionList";
import Department from "./component/DepartmentList/DepartmentList";
import Vaccine from "./component/VaccineList/VaccineList";

function App() {
    const GraphsWithLayout = withLayout(Graphs);
    const JurisdictionWithLayout = withLayout(Jurisdiction);
    const DepartmentWithLayout = withLayout(Department);
    const VaccineWithLayout = withLayout(Vaccine);
    return (
        <BrowserRouter>
            <Routes>
                <Route path={URLS.home} element={<GraphsWithLayout/>} />
                <Route path={URLS.jurisdiction} element={<JurisdictionWithLayout/>} />
                <Route path={URLS.department} element={<DepartmentWithLayout/>} />
                <Route path={URLS.vaccine} element={<VaccineWithLayout/>} />
            </Routes>
        </BrowserRouter>
    );
}

export default App;
