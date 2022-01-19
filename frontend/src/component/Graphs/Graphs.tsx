import React, { useState, useEffect } from "react";
// import useStyles from "./styles";
import { AreaChart, Area, XAxis, YAxis, Tooltip, LabelList } from 'recharts';
import Filter from "../../model/Filter/Filter";
import Pagination from "../../model/Pagination/Pagination";
import DateRepository from "../../repository/DateRepository";

const repositoryDate = new DateRepository();

export default function Graph() {

    // const classes = useStyles();

    const [dates, setDates] = useState<any[]>([])

    useEffect(() => {
        repositoryDate.list(new Pagination(10000, 0), new Filter())
            .then((r) => setDates(r))
            .catch((error) => console.log(error)) //TODO
    }, [])

    const renderCustomizedActiveDot = (props: any) => {
        const { cx, cy, stroke, dataKey } = props;

        return <path d={`M${cx - 2},${cy - 2}h4v4h-4Z`} fill={stroke} key={`dot-${dataKey}`} />;
    };

    return (
        <>
            <AreaChart width={2000} height={600} data={dates}
                margin={{ top: 20, right: 80, left: 20, bottom: 5 }}>
                <XAxis dataKey={"date"} angle={-45} textAnchor="end" height={100}/>
                <YAxis />
                <Tooltip />
                <Area stackId="0" type="monotone" dataKey="firstDose" stroke="#ff7300" fill="#ff7300" dot activeDot={renderCustomizedActiveDot} >
                    <LabelList position="bottom" />
                </Area>
                <Area stackId="0" type="monotone" dataKey="secondDose" stroke="#82ca9d" fill="#82ca9d" dot activeDot={renderCustomizedActiveDot} >
                    <LabelList />
                </Area>
                <Area stackId="0" type="monotone" dataKey="thirdDose" stroke="#387908" fill="#387908" animationBegin={1300} dot activeDot={renderCustomizedActiveDot} >
                    <LabelList position="top" />
                </Area>
            </AreaChart>
        </>
    );
}
