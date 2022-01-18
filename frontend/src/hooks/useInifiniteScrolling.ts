import { useEffect, useState } from "react";
import Pagination from "./../model/Pagination/Pagination";
import Filter from "./../model/Filter/Filter";

const useInfiniteScrolling = (getPage: (handleNewList: ((newsList: any[]) => void),
    pagination: Pagination, filter: Filter) => void, filter: Filter, pageSize: number):
    [boolean, Pagination, Filter, (f: Filter) => void, any[]] => {

    const [newsList, setNewList] = useState<any[]>([]);
    const [pageState, setPageState] = useState({
        pagination: new Pagination(pageSize, 0),
        filter: filter,
        loading: true,
        isFinish: false
    })
    const setFilter = (f: Filter) => {
        setPageState({
            ...pageState,
            filter: f,
        })
    }

    useEffect(() => {
        const defaultPagination = new Pagination(pageSize, 0)
        setPageState(p => {
            getPage((newsListResponce) => {
                setNewList(newsListResponce);
                setPageState(p => {
                    return {
                        ...p,
                        pagination: defaultPagination,
                        loading: false,
                        isFinish: newsListResponce.length === 0
                    }
                })
            }, defaultPagination, pageState.filter);
            setNewList([])
            return {
                ...p,
                pagination: defaultPagination,
                loading: true,
            }
        })
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [pageState.filter]);

    useEffect(() => {
        const handleScroll = () => {
            setPageState(p => {
                if ((window.innerHeight + window.scrollY) >= document.body.offsetHeight && !p.loading && !p.isFinish) {
                    const newPagination = p.pagination.advancePage();
                    getPage((newsListResponce) => {
                        setNewList(n => [...n, ...newsListResponce]);
                        setPageState(p => {
                            return {
                                ...p, loading: false,
                                isFinish: newsListResponce.length === 0
                            }
                        });
                    }, newPagination, p.filter);
                    return {
                        ...p,
                        pagination: newPagination,
                        loading: true,
                    }
                }
                return p;
            })

        }

        window.addEventListener('scroll', handleScroll);
        return () => window.removeEventListener('scroll', handleScroll);
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    return [pageState.loading, pageState.pagination, pageState.filter, setFilter, newsList]
}

export default useInfiniteScrolling;
