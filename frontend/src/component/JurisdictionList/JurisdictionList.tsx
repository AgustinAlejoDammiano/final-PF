import React, { useState } from "react";
import useStyles from "./styles";
import Pagination from "./../../model/Pagination/Pagination";
import Filter from "./../../model/Filter/Filter";
import Modal from "./../Modal/Modal";
import { CircularProgress, Container, Grid, Fab } from '@material-ui/core'
import AddIcon from '@material-ui/icons/Add';
import useInfiniteScrolling from "../../hooks/useInifiniteScrolling";
import JurisdictionRepository from "../../repository/JurisdictionRepository";
import JurisdictionFrom from "../JurisdictionFrom/JurisdictionFrom";

const repository = new JurisdictionRepository()

export default function JurisdictionList() {
    const PAGE_SIZE = 20;

    const getPage = async (handleNewList: ((jurisdictionList: any[]) => void),
        pagination: Pagination, filter: Filter) => {
        repository.list(pagination, filter)
            .then((jurisdictionListResponce) => {
                handleNewList(jurisdictionListResponce);
            })
            .catch((error) => console.log(error)) //TODO
    }

    const classes = useStyles();
    const [loading, , , , jurisdictionList] = useInfiniteScrolling(getPage, new Filter(), PAGE_SIZE)
    const [isModalOpen, setIsModalOpen] = useState(false);

    const onClose = () => { setIsModalOpen(false) }
    const onOpen = () => { setIsModalOpen(true) }

    const renderJurisdiction = (jurisdiction: any) => {
        return (
            <Grid container className={classes.gridContainer} spacing={3}>
                <Grid item>
                    {jurisdiction.id}
                </Grid>
                <Grid item>
                    {jurisdiction.name}
                </Grid>
            </Grid>
        )
    }

    const renderTitles = () => {
        return (
            <Grid container className={classes.gridContainer} spacing={3}>
                <Grid item>
                    ID
                </Grid>
                <Grid item>
                    Name
                </Grid>
            </Grid>
        )
    }

    const title = "Jurisdiction Form"

    const form = <JurisdictionFrom/>

    return (
        <>
            <Container maxWidth={false} className={classes.root}>
                <Grid container direction="column" className={classes.gridContainer} spacing={3}>
                    <Grid item>{renderTitles()}</Grid>
                    {jurisdictionList.map(jurisdiction => <Grid item>{renderJurisdiction(jurisdiction)}</Grid>)}
                </Grid>
                <Grid container direction="column" className={classes.gridContainer} spacing={3}>
                    <Grid item>
                        <Grid container justify="center" className={classes.gridContainer} spacing={3}>
                            {loading && <Grid item><CircularProgress color="primary" size={200} /></Grid>}
                        </Grid>
                    </Grid>
                </Grid>
            </Container>
            <Fab color="primary" aria-label="add" className={classes.fab} onClick={onOpen}>
                <AddIcon />
            </Fab>
            <Modal isOpen={isModalOpen} onClose={onClose} title={title}>
                {form}
            </Modal>
        </>
    )
}
