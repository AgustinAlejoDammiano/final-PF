import React from 'react';
import { BrowserRouter, Route, Routes } from "react-router-dom";
import { URLS } from "./constant/urls"
import withLayout from "./component/Layout/withLayout";
import Graphs from "./component/Graphs/Graphs";
import Jurisdiction from "./component/JurisdictionList/JurisdictionList";
import Department from "./component/DepartmentList/DepartmentList";
import Vaccine from "./component/VaccineList/VaccineList";
import Dose from "./component/DoseList/DoseList";

function App() {
    const GraphsWithLayout = withLayout(Graphs);
    const JurisdictionWithLayout = withLayout(Jurisdiction);
    const DepartmentWithLayout = withLayout(Department);
    const VaccineWithLayout = withLayout(Vaccine);
    const DoseWithLayout = withLayout(Dose);
    return (
        <BrowserRouter>
            <Routes>
                <Route path={URLS.home} element={<GraphsWithLayout/>} />
                <Route path={URLS.jurisdiction} element={<JurisdictionWithLayout/>} />
                <Route path={URLS.department} element={<DepartmentWithLayout/>} />
                <Route path={URLS.vaccine} element={<VaccineWithLayout/>} />
                <Route path={URLS.dose} element={<DoseWithLayout/>} />
            </Routes>
        </BrowserRouter>
    );
}

export default App;
