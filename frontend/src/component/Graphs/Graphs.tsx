import React, { useState, useEffect } from "react";
// import useStyles from "./styles";
import { AreaChart, Area, XAxis, YAxis, Tooltip, LabelList, CartesianGrid, BarChart, Bar, Legend } from 'recharts';
import Filter from "../../model/Filter/Filter";
import Pagination from "../../model/Pagination/Pagination";
import DateRepository from "../../repository/DateRepository";
import JurisdictionRepository from "../../repository/JurisdictionRepository";

const repositoryDate = new DateRepository();
const repositoryJurisdiction = new JurisdictionRepository();

export default function Graph() {

    // const classes = useStyles();

    const [dates, setDates] = useState<any[]>([])
    const [jurisdiction, setJurisdiction] = useState<any[]>([])

    useEffect(() => {
        repositoryDate.list(new Pagination(10000, 0), new Filter())
            .then((r) => setDates(r))
            .catch((error) => console.log(error)) //TODO
        repositoryJurisdiction.listDose(new Pagination(10000, 0), new Filter())
            .then((r) => setJurisdiction(r))
            .catch((error) => console.log(error)) //TODO
    }, [])

    const renderCustomizedActiveDot = (props: any) => {
        const { cx, cy, stroke, dataKey } = props;

        return <path d={`M${cx - 2},${cy - 2}h4v4h-4Z`} fill={stroke} key={`dot-${dataKey}`} />;
    };

    return (
        <div>
            <AreaChart width={2000} height={600} data={dates}
                margin={{ top: 20, right: 80, left: 20, bottom: 5 }}>
                <XAxis dataKey={"date"} angle={-45} textAnchor="end" height={100} />
                <YAxis />
                <Tooltip />
                <Area stackId="0" type="monotone" dataKey="firstDose" stroke="#ff7300" 
                    fill="#ff7300" dot activeDot={renderCustomizedActiveDot} />
                <Area stackId="0" type="monotone" dataKey="secondDose" stroke="#82ca9d" 
                    fill="#82ca9d" dot activeDot={renderCustomizedActiveDot} />
                <Area stackId="0" type="monotone" dataKey="thirdDose" stroke="#387908" 
                    fill="#387908" animationBegin={1300} dot activeDot={renderCustomizedActiveDot} />
            </AreaChart>
            <BarChart width={2000} height={800} data={jurisdiction} layout="vertical">
                <XAxis type="number" />
                <YAxis dataKey="name" type="category" width={100}/>
                <Tooltip />
                <CartesianGrid vertical={false} />
                <Bar stackId="0" dataKey="firstDose" fill="#ff7300"/>
                <Bar stackId="0" dataKey="secondDose" fill="#82ca9d"/>
                <Bar stackId="0" dataKey="thirdDose" fill="#387908"/>
                <Legend layout="horizontal" />
            </BarChart>
        </div>
    );
}
