import React, { useState } from "react";
import useStyles from "./styles";
import Pagination from "./../../model/Pagination/Pagination";
import Filter from "./../../model/Filter/Filter";
import { Grid, Button, Typography } from '@material-ui/core'
import DeleteIcon from '@material-ui/icons/Delete';
import JurisdictionRepository from "../../repository/JurisdictionRepository";
import JurisdictionFrom from "../JurisdictionFrom/JurisdictionFrom";
import withList from "../List/withList";

const repository = new JurisdictionRepository()

export default function JurisdictionList() {

    const [deleteJurisdictions, setDeleteJurisdictions] = useState<any[]>([])

    const getPage = async (handleNewList: ((jurisdictionList: any[]) => void),
        pagination: Pagination, filter: Filter) => {
        repository.list(pagination, filter)
            .then((jurisdictionListResponce) => {
                handleNewList(jurisdictionListResponce);
            })
            .catch((error) => console.log(error)) //TODO
    }

    const deleteJurisdiction = (jurisdiction: any) => {
        return () => {
            repository.delete(jurisdiction.id)
                .then(() => {
                    setDeleteJurisdictions((l) => [...l, jurisdiction])
                })
                .catch((error) => console.log(error)) //TODO
        }
    }

    const onDelete = () => {
        repository.deleteAll()
                .then(() => window.location.reload())
                .catch((error) => console.log(error)) //TODO
    }

    const classes = useStyles();

    const renderJurisdiction = (jurisdiction: any) => {
        if (deleteJurisdictions.includes(jurisdiction)) {
            return null
        }
        return (
            <Grid container className={classes.gridContainer} spacing={3}>
                <Grid item><Typography variant="body2">{jurisdiction.id}</Typography></Grid>
                <Grid item><Typography variant="body2">{jurisdiction.name}</Typography></Grid>
                <Grid item>
                    <Button size="small" color="primary" onClick={deleteJurisdiction(jurisdiction)}>
                        <DeleteIcon />
                    </Button>
                </Grid>
            </Grid>
        )
    }

    const renderTitles = () => {
        return (
            <Grid container className={classes.gridContainer} spacing={3}>
                <Grid item><Typography variant="h5">ID</Typography></Grid>
                <Grid item><Typography variant="h5">Name</Typography></Grid>
                <Grid item>
                    <Button size="small" color="primary" disabled></Button>
                </Grid>
            </Grid>
        )
    }

    const title = "Jurisdiction Form"

    const JurisdictionList = withList(JurisdictionFrom, getPage, renderTitles, renderJurisdiction, title, onDelete)

    return <JurisdictionList />
}
